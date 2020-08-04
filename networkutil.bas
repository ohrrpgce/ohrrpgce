' OHRRPGCE common - Berkeley socket-based networking routines (Windows and Unix)
' See also networkutil.bas for related code.
' Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "os.bi"

dim shared as string boundary '= "c6309069605c1e802d96cf"
boundary = STR(RND)

extern "C"

sub POST_add_text_part(byref buffer as string, name as string, contents as string)
	buffer &= !"\r\n--" & boundary & !"\r\nContent-Disposition: form-data; name=""" & name & !"""\r\n\r\n" & contents
end sub

sub POST_add_file_part(byref buffer as string, name as string, filename as string, content_type as string, contents as string)
	buffer &= !"\r\n--" & boundary & !"\r\nContent-Disposition: form-data; name=""" & name & _
	       !"""; filename=""" & filename & !"""\r\nContent-Type: " & content_type & !"\r\nContent-Transfer-Encoding: binary\r\n\r\n"
	buffer &= contents
end sub

'Must be called, once, after last POST_add_file_part/POST_add_text_part call
sub POST_add_final_part(byref buffer as string)
	buffer &= !"\r\n--" & boundary & !"--\r\n"
end sub

'This is a replacement for HTTP_request().
'req should be uninitialised, and destroyed afterwards with HTTP_Request_destroy().
'buffer is the contents of the message, which has been built up by calling POST_add_text_part() and
'POST_add_file_part() and then finally POST_add_final_part().
function multipart_POST_request(req as HTTPRequest ptr, url as string, buffer as string) as boolint
	dim as string content_type = "multipart/form-data; boundary=""" & boundary & """"
	return HTTP_request(req, url, "POST", content_type, @buffer[0], len(buffer))
end function

/'
sub test_POST
	dim buffer as string
	POST_add_text_part buffer, "input", "hi!"
	POST_add_text_part buffer, "more stuff", !"hello\nworld!"
	POST_add_final_part buffer

	dim url as string = "127.0.0.1:8001"
	dim req as HTTPRequest
	multipart_POST_request(@req, url, buffer)
	? "failed=" & req.failed & " (" & *(req.failmsg)  & ") status=" & req.status & " (" & *req.status_string & ")"
	? *req.response_buf
	HTTP_Request_destroy(@req)
end sub
'/

end extern
