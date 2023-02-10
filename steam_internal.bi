'OHRRPGCE GAME - typedefs for interfacing with Steamworks
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
#ifndef STEAM_INTERNAL_BI
#define STEAM_INTERNAL_BI

#ifdef __FB_WIN32__
#define STEAM_PACK 8
#else
#define STEAM_PACK 4
#endif

type ISteamUserStats as any
type HSteamPipe as integer
type HSteamUser as integer
type SteamAPICall_t as uint64
type CSteamID as uint64 ' there is more nuance to this, but this will do for now
type EResult as uint32

type CallbackMsg_t field = STEAM_PACK
    m_hSteamUser as HSteamUser
    m_iCallback as integer
    m_pubParam as ubyte ptr
    m_cubParam as integer
end type

type SteamAPICallCompleted_t field = STEAM_PACK
    const k_iCallback as integer = 703
    m_hAsyncCall as SteamAPICall_t
    m_iCallback as integer
    m_cubParam as uinteger
end type

type UserStatsReceived_t field = STEAM_PACK
    const k_iCallback as integer = 1101
    m_nGameId as uint64
    m_eResult as EResult
    m_steamIDUser as CSteamID
end type

declare sub OnUserStatsReceived(msg as UserStatsReceived_t ptr)

#endif
