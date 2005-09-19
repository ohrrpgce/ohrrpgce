option explicit

CONST attrib_readonly   = 1
CONST attrib_hidden     = 2
CONST attrib_system     = 4
CONST attrib_directory  = 16
CONST attrib_archive    = 32

#if 1
sub memcpy(byval dst as any ptr, byval src as any ptr, byval bytes as integer)
    dim _dst as byte ptr, _src as byte ptr
    dim i as integer
    _dst = dst
    _src = src
    for i = bytes to 0 step -1
        *_dst = *_src
        _dst += 1
        _src += 1
    next i
end sub
#else
#include "crt.bi"
#endif

type DirEx_t
    num_entries as integer
    entries as zstring ptr ptr
    next_entry as integer
end type

dim shared DirEx_mutex as integer

Function DirEx(byref handle as DirEx_t ptr, filespec as string = "", byval attribs as integer = 0) as string
dim res as string
dim i as integer
    if DirEx_mutex = 0 then
        DirEx_mutex = mutexcreate
    end if
    if handle = 0 then
        If DirEx_mutex <> 0 then mutexlock DirEx_mutex
        if attribs <> 0 then
            res = dir(filespec, attribs)
        else
            res = dir(filespec)
        end if
        if len(res) then
            handle = callocate(sizeof(DirEx_T))
            Function = res
            Do While Len(res)
                handle->num_entries += 1
                handle->entries = reallocate(handle->entries, sizeof(zstring ptr) * handle->num_entries)
                handle->entries[handle->num_entries - 1] = callocate(len(res) + 1)
                memcpy(handle->entries[handle->num_entries - 1], strptr(res), len(res) + 1)
                if attribs <> 0 then
                    res = dir("", attribs)
                else
                    res = dir()
                end if
            Loop
        else
            Function = ""
        end if
        If DirEx_mutex <> 0 then mutexunlock DirEx_mutex
    else
        if handle->next_entry = handle->num_entries then
            function = ""
            ' deallocate everything if this is the last entry
            for i = handle->num_entries - 1 to 0 step -1
                deallocate handle->entries[i]
            next i
            deallocate handle->entries
            deallocate handle
            handle = 0
        else
            ' enumerate results from already allocated handle
            function = *handle->entries[handle->next_entry]
            handle->next_entry += 1
        end if
    end if
end function