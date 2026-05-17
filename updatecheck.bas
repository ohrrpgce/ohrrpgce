'OHRRPGCE CUSTOM - Check for engine updates
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "util.bi"
#include "common.bi"
#include "os.bi"
#include "datetime.bi"


type ReleaseInfo
	release_name as string 'Codename+minor, like the (badly named) version_branch global
	display_name as string 'Same as release_name except for hróðvitnir
	date as integer        'YYYYMMDD, like version_date global
	datestr as string      'YYYY-MM-DD
	update_for as string   'The major
	description as string
	notice as string
end type

declare sub parse_releases(content as string, releases() as ReleaseInfo)


' Parse releases.txt, returning list of releases. Ignores other metadata.
sub parse_releases(content as string, releases() as ReleaseInfo)
	dim lines() as string
	split content, lines(), chr(10)

	for i as integer = 0 to ubound(lines)
		dim curline as string = ltrim(lines(i))
		if left(curline, 1) = "#" then continue for

		dim eq_pos as integer = instr(curline, "=")
		if eq_pos = 0 then continue for

		dim key as string = trim(left(curline, eq_pos - 1))
		dim value as string = trim(mid(curline, eq_pos + 1))

		dim keyparts() as string
		split key, keyparts(), "."
		if ubound(keyparts) <> 2 then continue for
		if keyparts(0) <> "release" then continue for

		dim release_name as string = keyparts(1)
		dim datafield as string = keyparts(2)

		dim release_idx as integer = -1
		for j as integer = 0 to ubound(releases)
			if releases(j).release_name = release_name then
				release_idx = j
				exit for
			end if
		next

		if release_idx = -1 then
			' Add release
			redim preserve releases(ubound(releases) + 1)
			release_idx = ubound(releases)
			releases(release_idx).release_name = release_name
		end if

		with releases(release_idx)
			if datafield = "name" then
				.display_name = value
			elseif datafield = "date" then
				.datestr = value
				replacestr(value, "-", "")
				.date = valint(value)
			elseif datafield = "update_for" then
				.update_for = value
			elseif datafield = "description" then
				.description = value
			elseif datafield = "notice" then
				.notice = value
			end if
		end with
	next
end sub

' always_show: show the update even if it's been shown in the past
function check_for_updates (always_show as bool = NO) as string
	dim releases_file as string = settings_dir & SLASH & "releases.txt"
	dim content as string
	dim success as bool

	if isfile(releases_file) then
		' How old the cached file is, in minutes
		dim cache_age as double = 24 * 60 * (now - filedatetime(releases_file))
		if cache_age < 0.1 then
			content = string_from_file(releases_file, YES, success)
			if not success then
				return "Failed to read cached releases.txt"
			end if
		end if
	end if

	' Fetch the file directly rather than using wget or curl, because popping up a window is quite awful.
	' However this might cause Windows to prompt the user to allow it.

	if content = "" then
		dim req as HTTPRequest
		dim url as string = "http://hamsterrepublic.com/ohrrpgce/archive/releases_test.txt"
		debuginfo "Downloading " & url
		if nogfx_mode then print "Downloading " & url
		if HTTP_request(@req, url, "GET") = NO then
			HTTP_Request_destroy(@req)
			return "Failed to download release list."
		end if

		if req.status <> 200 then
			dim msg as string = "Failed to download release list: " & req.status & " " & *req.status_string
			HTTP_Request_destroy(@req)
			return msg
		end if

		content = *cast(zstring ptr, req.response)
		HTTP_Request_destroy(@req)
		string_to_file(content, releases_file)
	end if

	dim releases() as ReleaseInfo
	parse_releases content, releases()
	dim message as string = ""

	dim last_major_date as integer
	dim last_major_desc as string
	dim last_minor_date as integer
	dim last_minor_desc as string
	dim last_notice as string

	if always_show = NO then
		last_major_date = read_config_int("update_checks.next_major.date")
		last_major_desc = read_config_str("update_checks.next_major.description")
		last_minor_date = read_config_int("update_checks.next_minor.date")
		last_minor_desc = read_config_str("update_checks.next_minor.description")
		last_notice = read_config_str("update_checks.last_notice")
	end if

	' The current version with "+#" minor version tag stripped
	dim current_major as string = version_branch
	dim plusidx as integer = instr(current_major, "+")
	if plusidx then current_major = left(current_major, plusidx - 1)

	' Don't trust the version_date global, because if someone has compiled the engine
	' themeselves it may not be actual release date of the the current version.
	' Instead, look up the official release date.
	' If this version_branch is "wip" then we really do care about build date.
	dim current_date as integer = version_date
	for i as integer = 0 to ubound(releases)
		with releases(i)
			if .release_name = version_branch then
				current_date = .date
			end if
		end with
	next

	' We assume the file is sorted by ascending date, and will only show the newest major and minor
	' releases available

	' dim newest_major as integer   'Index in releases()
	' dim newest_minor as integer   'Index in releases()

	for i as integer = ubound(releases) to 0 step -1
		with releases(i)
			if .date > current_date then ' Newer

				if .update_for = "" then ' Major release
					if always_show or .date > last_major_date or (.date = last_major_date and .description <> last_major_desc) then
						last_major_date = .date
						message &= !"New major release available!\n"
						message &= .display_name & " released " & .datestr & !"\n"
						if .description <> "" then
							message &= "  " & .description & !"\n"
						end if
						write_config "update_checks.next_major.date", .date
						write_config "update_checks.next_major.description", .description
					end if
				elseif .update_for = current_major then ' Minor release for current version
					if always_show or .date > last_minor_date or (.date = last_minor_date and .description <> last_minor_desc) then
						last_minor_date = .date
						message &= !"New minor release available for your version!\n"
						message &= .display_name & " released " & .datestr & !"\n"
						if .description <> "" then
							message &= "  " & .description & !"\n"
						end if
						write_config "update_checks.next_minor.date", .date
						write_config "update_checks.next_minor.description", .description
					end if
				end if
			end if

			if .release_name = version_branch and .notice <> "" and .notice <> last_notice then
				' Note that notices for "wip" are possible
				message &= !"Notice for your current version (" & .display_name & !"):\n" & .notice & !"\n"
				write_config "update_checks.last_notice", .notice
			end if
		end with
	next

	if message = "" andalso always_show then
		return "No OHRRPGCE updates found; you're using the latest."
	end if

	return message
end function
