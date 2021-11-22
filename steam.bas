'OHRRPGCE GAME - code for interfacing with Steamworks
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
#include "common_base.bi"
#include "steam.bi"
#include "steam_internal.bi"

' #define DEBUG_STEAM

namespace Steam

#ifdef DEBUG_STEAM
declare sub steam_debug(msg as string)
#else
#define steam_debug(x)
#endif

' event handlers

declare sub OnUserStatsReceived(msg as UserStatsReceived_t ptr)

' static variables

dim shared steamworks_handle as any ptr = null

' reminder: these are all function pointers, not function declarations
' basic init/deinit
dim shared SteamAPI_Init as function() As boolean
dim shared SteamAPI_Shutdown as sub()
dim shared SteamAPI_RestartAppIfNecessary as function( byval unOwnAppID as uinteger ) as boolean

' callback infrastructure
dim shared SteamAPI_GetHSteamPipe as function() as HSteamPipe
dim shared SteamAPI_ManualDispatch_Init as sub()
dim shared SteamAPI_ManualDispatch_RunFrame as sub( byval hSteamPipe as HSteamPipe )
dim shared SteamAPI_ManualDispatch_GetNextCallback as function ( hSteamPipe as HSteamPipe, pCallbackMsg as CallbackMsg_t ptr) as boolean
dim shared SteamAPI_ManualDispatch_FreeLastCallback as sub ( hSteamPipe as HSteamPipe)
dim shared SteamAPI_ManualDispatch_GetAPICallResult as function ( hSteamPipe as HSteamPipe, hSteamAPICall as SteamAPICall_t, pCallback as any ptr, cubCallback as integer,  iCallbackExpected as integer, pbFAiled as boolean ptr) as boolean

' achievements
dim shared SteamAPI_SteamUserStats_v012 as function () as ISteamUserStats ptr
dim shared SteamAPI_ISteamUserStats_RequestCurrentStats as function(byval self as ISteamUserStats ptr) as boolean
dim shared SteamAPI_ISteamUserStats_SetAchievement as function(byval self as ISteamUserStats ptr, byval name as const zstring ptr) as boolean
dim shared SteamAPI_ISteamUserStats_ClearAchievement as function(byval self as ISteamUserStats ptr, byval name as const zstring ptr) as boolean
dim shared SteamAPI_ISteamUserStats_StoreStats as function(byval self as ISteamUserStats ptr) as boolean
dim shared SteamAPI_ISteamUserStats_IndicateAchievementProgress as function(byval self as ISteamUserStats ptr, byval name as const zstring ptr, progress as uinteger, max_progress as uinteger) as boolean


dim shared steam_user_stats as ISteamUserStats ptr

#macro MUSTLOAD(hfile, procedure)
  procedure = dylibsymbol(hfile, #procedure)
  if procedure = NULL then
    steam_debug("Was not able to find " & #procedure)
    ' do this instead of uninitialize_steam(), since it assumes we succeeded in initializing
    dylibfree(hFile)
    hFile = null
    return NO
  end if
#endmacro

function initialize() as boolean

  #ifdef __FB_WIN32__
    #ifdef __FB_64BIT__
      'Future, although not yet supported
      #define STEAM_LIB "steam_api64"
      #define STEAM_FULL_FNAME "steam_api64.dll"
    #else
      #define STEAM_LIB "steam_api"
      #define STEAM_FULL_FNAME "steam_api.dll"
    #endif
  #elseif defined(__FB_DARWIN__)
    #define STEAM_LIB "steam_api"
    #define STEAM_FULL_FNAME "libsteam_api.dylib"
  #else
    'Either Linux or maybe a BSD (on which Steam isn't officially support but can be run)
    #define STEAM_LIB "steam_api"
    #define STEAM_FULL_FNAME "libsteam_api.so"
  #endif

  steamworks_handle = dylibload(STEAM_LIB)
  if steamworks_handle = null then
    steam_debug("Was not able to open " STEAM_FULL_FNAME)
    return false
  end if

  MUSTLOAD(steamworks_handle, SteamAPI_Init)
  MUSTLOAD(steamworks_handle, SteamAPI_Shutdown)
  MUSTLOAD(steamworks_handle, SteamAPI_RestartAppIfNecessary)
  MUSTLOAD(steamworks_handle, SteamAPI_GetHSteamPipe)
  MUSTLOAD(steamworks_handle, SteamAPI_ManualDispatch_Init)
  MUSTLOAD(steamworks_handle, SteamAPI_ManualDispatch_RunFrame)
  MUSTLOAD(steamworks_handle, SteamAPI_ManualDispatch_GetNextCallback)
  MUSTLOAD(steamworks_handle, SteamAPI_ManualDispatch_FreeLastCallback)
  MUSTLOAD(steamworks_handle, SteamAPI_ManualDispatch_GetAPICallResult)
  MUSTLOAD(steamworks_handle, SteamAPI_SteamUserStats_v012)
  MUSTLOAD(steamworks_handle, SteamAPI_ISteamUserStats_RequestCurrentStats)
  MUSTLOAD(steamworks_handle, SteamAPI_ISteamUserStats_SetAchievement)
  MUSTLOAD(steamworks_handle, SteamAPI_ISteamUserStats_ClearAchievement)
  MUSTLOAD(steamworks_handle, SteamAPI_ISteamUserStats_StoreStats)
  MUSTLOAD(steamworks_handle, SteamAPI_ISteamUserStats_IndicateAchievementProgress)
  
  if SteamAPI_Init() = false then
    steam_debug("unable to initialize steamworks")
    uninitialize()
    return false
  end if

  ' todo: is this necessary?
  ' if SteamAPI_RestartAppIfNecessary( ourAppId ) <> false then
  '     steam_debug("Steam seems to want to restart the application for some reason")
  ' end if

  SteamAPI_ManualDispatch_Init()

  ' all stuff to do with stats and achievements go through SteamUserStats interface:
  steam_user_stats = SteamAPI_SteamUserStats_v012()

  if steam_user_stats = null then
    steam_debug("Unable to obtain user stats object")
  else
    ' we need to instruct steam to fetch the user stats, so we can reward achievements later
    if SteamAPI_ISteamUserStats_RequestCurrentStats(steam_user_stats) = false then
      steam_debug("Unable to request current stats")
    end if
  end if

  return true

end function

sub uninitialize()
  if steamworks_handle <> null then
    dylibfree(steamworks_handle)
    steamworks_handle = null
  end if
end sub

function available() as boolean
  return steamworks_handle <> null
end function

sub reward_achievement(id as const string)
  if available() = false then return

  if SteamAPI_ISteamUserStats_SetAchievement(steam_user_stats, id) = false then
    steam_debug("unable to reward achievement: " & id)
  else
    if SteamAPI_ISteamUserStats_StoreStats(steam_user_stats) = false then
      steam_debug("unable to persist stats")
    end if
  end if
end sub

sub clear_achievement(id as string)
  if available() = false then return

  if SteamAPI_ISteamUserStats_ClearAchievement(steam_user_stats, id) = false then
    steam_debug("unable to clear an achievement: " & id)
  end if
end sub

sub notify_achievement_progress(id as const string, progress as integer, max_progress as integer)
  if available() = false then return

  if SteamAPI_ISteamUserStats_IndicateAchievementProgress(steam_user_stats, id, progress, max_progress) = false then
    steam_debug("unable to indicate achievement progress: " & id)
  end if
end sub

#macro CALLBACK_HANDLER(typ, handler)
  case typ.k_iCallback
    steam_debug(#typ ", message length: " & callback.m_cubParam)
    dim typ##Msg as typ ptr = cast(typ ptr, callback.m_pubParam)
    handler(typ##Msg)
#endmacro

#macro IGNORE(id)
  case id
    ' steam_debug("Ignored Steam id: " & id)
#endmacro

dim shared achieve_timer as integer = -1

sub run_frame()
  if available() = false then return

  ' steam_debug("run_steam_frame")

  dim hSteamPipe as HSteamPipe = SteamAPI_GetHSteamPipe()
  SteamAPI_ManualDispatch_RunFrame(hSteamPipe)
  dim callback as CallbackMsg_t
  while SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe, @callback)
    ' Check for dispatching API call results
    if callback.m_iCallback = 703 then
      dim pCallCompleted as SteamAPICallCompleted_t ptr = cast(SteamAPICallCompleted_t ptr, @callback)
      dim pTmpCallResult as any ptr = allocate(pCallCompleted->m_cubParam)
      dim bFailed as boolean
      if SteamAPI_ManualDispatch_GetAPICallResult ( hSteamPipe, pCallCompleted->m_hAsyncCall, pTmpCallResult, pCallCompleted->m_cubParam, pCallCompleted->m_iCallback, @bFailed ) then
        ' Dispatch the call result to the registered handler(s) for the
        ' call identified by pCallCompleted->m_hAsyncCall
        steam_debug("Call Completed handler")
      end if
      deallocate(pTmpCallResult)
    else
      ' Look at callback.m_iCallback to see what kind of callback it is,
      ' and dispatch to appropriate handler(s)
      select case callback.m_iCallback
        CALLBACK_HANDLER(UserStatsReceived_t, OnUserStatsReceived)
        ' these are messages that we either don't know the identity of, or we don't care
        IGNORE(715)
        IGNORE(304)
        IGNORE(711)
        IGNORE(903)
        IGNORE(501)
        IGNORE(502) ' favorites list changed
        IGNORE(1006)
        IGNORE(1102) ' user stats stored
        case else
          steam_debug("Some other handler: " & callback.m_iCallback)
      end select
    end if
    SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe)
  wend
end sub

private sub OnUserStatsReceived(msg as UserStatsReceived_t ptr)
  steam_debug("On User Stats Received")

  ' unsure if we actually need to do anything in response to this.
  ' TODO: buffer any achievement activity that happens before this call?

  ' achieve_timer = 1000
end sub

#ifdef DEBUG_STEAM
sub steam_debug(msg as string)
  debug "steam: " & msg
end sub
#endif

end namespace