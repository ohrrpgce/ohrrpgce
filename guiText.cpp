#include <string.h>
#include "guiText.h"

enum TextCodes
{
	TC_CHAR,
	TC_COLOR_PUSH,
	TC_COLOR_POP,
	TC_WIDTH_PUSH,
	TC_WIDTH_POP,
	TC_HEIGHT_PUSH,
	TC_HEIGHT_POP,
	TC_SIZE_PUSH,
	TC_SIZE_POP,
	TC_ALIGN_PUSH,
	TC_ALIGN_POP,
	TC_NEWLINE,
	TC_TAB,
	TC_SPACE,
};

enum Alignment
{
	ALIGN_LEFT,
	ALIGN_CENTER,
	ALIGN_RIGHT,
	ALIGN_JUSTIFIED,
	ALIGN_TOP = ALIGN_LEFT,
	ALIGN_MIDDLE = ALIGN_CENTER,
	ALIGN_BOTTOM = ALIGN_RIGHT,
};

void CGuiText::CharRegion::finalize()
{
	v[0].c = color;
	v[0].u = texCoord.left;
	v[0].v = texCoord.top;
	v[0].x = region.left;
	v[0].y = region.bottom - height;
	v[0].z = 0.0f;

	v[1].c = color;
	v[1].u = texCoord.right;
	v[1].v = texCoord.top;
	v[1].x = region.left + width;
	v[1].y = region.bottom - height;
	v[1].z = 0.0f;

	v[2].c = color;
	v[2].u = texCoord.right;
	v[2].v = texCoord.bottom;
	v[2].x = region.left + width;
	v[2].y = region.bottom;
	v[2].z = 0.0f;

	v[3].c = color;
	v[3].u = texCoord.left;
	v[3].v = texCoord.bottom;
	v[3].x = region.left;
	v[3].y = region.bottom;
	v[3].z = 0.0f;
}




CGuiText::CGuiText(FontObject *pFont, ConstantBuffer *cbuffer, IndexBuffer *ibuffer)
: m_cb(cbuffer), m_vb(0), m_ib(ibuffer), m_font(pFont), m_defaultColor(0xffaaaaaa), m_defaultHeight(30), 
m_defaultWidth(25), m_alphaOverride(0xff000000), m_defaultHAlign(ALIGN_LEFT), m_defaultVAlign(ALIGN_TOP),
m_isMultiline(false), m_isAutoWrap(false), m_isClipping(false), m_allowsGuiTextCode(true), m_allowsCaret(false),
m_isSelectable(false)
{
	m_caretFlash.setLength( 500 );
}

CGuiText::~CGuiText()
{
}

bool CGuiText::isHexNumber(const char *szText, int numDigits) const
{
	for(int i = 0; i < numDigits; i++)
	{
		if( szText[i] == 0 )
			return false;
		else if( (szText[i] > '9' || szText[i] < '0') && (szText[i] > 'F' && szText[i] < 'A') )
			return false;
	}
	return true;
}

unsigned int CGuiText::atohex(const char *szText) const
{
	unsigned int result = 0;
	unsigned int temp = 0;
	for(int i = 0; i < 8; i++)
	{
		if(szText[i] == 0)
			break;
		temp = szText[i] - '0';
		if(temp > 9)
			temp = szText[i] - 'A' + 10;
		temp <<= ((7-i) * 4);
		result += temp;
	}
	return result;
}

void CGuiText::extract(const char *szText)
{
	if(szText == 0)
		return;

	while(*szText != 0)
	{
		CharCode cc;
		szText += textToCode(cc, szText);
		m_text.push_back( cc );
	}
	updateRegions();
}

unsigned int CGuiText::textToCode(CGuiText::CharCode &ccOut, const char *p) const
{
	if( *p == '<' && m_allowsGuiTextCode ) //possibly beginning of code, and code is allowed
	{
		switch( *(p + 1) )
		{
		case 'c':
		case 'C':
			{
				//compare "COLOR=" in all caps
				const char* q = p + 1;
				char buffer[16] = "";
				for(int i = 0; i < 6; i++)
				{
					buffer[i] = toupper( *(q + i) );
					if( buffer[i] == 0 )
						break;
				}

				//extract hex color in all caps
				q += 6;
				if( strcmp( buffer, "COLOR=" ) == 0 ) //they are equivalent
				{
					memset( buffer, 0, sizeof(buffer) );
					for(int i = 0; i < 8; i++)
					{
						buffer[i] = toupper( *(q + i) );
						if( buffer[i] == 0 )
							break;
					}

					if( isHexNumber( buffer ) )
					{
						q += 8;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = atohex( buffer );
						ccOut.code = TC_COLOR_PUSH;
						return q - p;
					}
				}
			} break;
		case 'w':
		case 'W':
			{
				//compare "WIDTH=" in all caps
				const char* q = p + 1;
				char buffer[16] = "";
				for(int i = 0; i < 6; i++)
				{
					buffer[i] = toupper( *(q + i) );
					if( buffer[i] == 0 )
						break;
				}

				//extract decimal size
				q += 6;
				if( strcmp( buffer, "WIDTH=" ) == 0 ) //they are equivalent
				{
					memset( buffer, 0, sizeof(buffer) );
					int i = 0;
					while( *(q + i) != 0 && isdigit( *(q + i) ) && i < sizeof(buffer))
					{
						buffer[i] = *(q + i);
						i++;
					}

					if(buffer[0] != 0)
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = atoi( buffer );
						ccOut.code = TC_WIDTH_PUSH;
						return q - p;
					}
				}
			} break;
		case 'h':
		case 'H':
			{
				//compare "HEIGHT=" in all caps
				const char* q = p + 1;
				char buffer[16] = "";
				for(int i = 0; i < 7; i++)
				{
					buffer[i] = toupper( *(q + i) );
					if( buffer[i] == 0 )
						break;
				}

				//extract decimal size
				q += 7;
				if( strcmp( buffer, "HEIGHT=" ) == 0 ) //they are equivalent
				{
					memset( buffer, 0, sizeof(buffer) );
					int i = 0;
					while( *(q + i) != 0 && isdigit( *(q + i) ) && i < sizeof(buffer))
					{
						buffer[i] = *(q + i);
						i++;
					}

					if(buffer[0] != 0)
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = atoi( buffer );
						ccOut.code = TC_HEIGHT_PUSH;
						return q - p;
					}
				}
			} break;
		case 's':
		case 'S':
			{
				//compare "SIZE=" in all caps
				const char* q = p + 1;
				char buffer[16] = "";
				for(int i = 0; i < 5; i++)
				{
					buffer[i] = toupper( *(q + i) );
					if( buffer[i] == 0 )
						break;
				}

				//extract decimal size
				q += 5;
				if( strcmp( buffer, "SIZE=" ) == 0 ) //they are equivalent
				{
					memset( buffer, 0, sizeof(buffer) );
					int i = 0;
					while( *(q + i) != 0 && isdigit( *(q + i) ) && i < sizeof(buffer))
					{
						buffer[i] = *(q + i);
						i++;
					}

					if(buffer[0] != 0)
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = atoi( buffer );
						ccOut.code = TC_SIZE_PUSH;
						return q - p;
					}
				}
			} break;
		case 'a':
		case 'A':
			{
				//compare "ALIGN=" in all caps
				const char* q = p + 1;
				char buffer[16] = "";
				for(int i = 0; i < 6; i++)
				{
					buffer[i] = toupper( *(q + i) );
					if( buffer[i] == 0 )
						break;
				}

				//extract decimal size
				q += 6;
				if( strcmp( buffer, "ALIGN=" ) == 0 ) //they are equivalent
				{
					memset( buffer, 0, sizeof(buffer) );
					int i = 0;
					while( *(q + i) != 0 && isalpha( *(q + i) ) && i < sizeof(buffer))
					{
						buffer[i] = *(q + i);
						i++;
					}

					if( strcmp( buffer, "LEFT" ) == 0 )
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = ALIGN_LEFT;
						ccOut.code = TC_ALIGN_PUSH;
						return q - p;
					}
					else if( strcmp( buffer, "RIGHT" ) == 0 )
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = ALIGN_RIGHT;
						ccOut.code = TC_ALIGN_PUSH;
						return q - p;
					}
					else if( strcmp( buffer, "CENTER" ) == 0 )
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = ALIGN_CENTER;
						ccOut.code = TC_ALIGN_PUSH;
						return q - p;
					}
					else if( strcmp( buffer, "JUSTIFIED" ) == 0 )
					{
						q += i;
						if( *q != '>' ) //must end with '>'
							break;
						q++;

						ccOut.c = ALIGN_JUSTIFIED;
						ccOut.code = TC_ALIGN_PUSH;
						return q - p;
					}
				}
			} break;
		case '/':
			{
				switch( *(p + 2) )
				{
				case 'c':
				case 'C':
					{
						//compare "COLOR>" in all caps
						const char* q = p + 2;
						char buffer[16] = "";
						for(int i = 0; i < 6; i++)
						{
							buffer[i] = toupper( *(q + i) );
							if( buffer[i] == 0 )
								break;
						}

						//extract command close
						q += 6;
						if( strcmp( buffer, "COLOR>" ) == 0 ) //they are equivalent
						{
							ccOut.c = 0;
							ccOut.code = TC_COLOR_POP;
							return q - p;
						}
					} break;
				case 'w':
				case 'W':
					{
						//compare "WIDTH>" in all caps
						const char* q = p + 2;
						char buffer[16] = "";
						for(int i = 0; i < 6; i++)
						{
							buffer[i] = toupper( *(q + i) );
							if( buffer[i] == 0 )
								break;
						}

						//extract command close
						q += 6;
						if( strcmp( buffer, "WIDTH>" ) == 0 ) //they are equivalent
						{
							ccOut.c = 0;
							ccOut.code = TC_WIDTH_POP;
							return q - p;
						}
					} break;
				case 'h':
				case 'H':
					{
						//compare "HEIGHT>" in all caps
						const char* q = p + 2;
						char buffer[16] = "";
						for(int i = 0; i < 7; i++)
						{
							buffer[i] = toupper( *(q + i) );
							if( buffer[i] == 0 )
								break;
						}

						//extract command close
						q += 7;
						if( strcmp( buffer, "HEIGHT>" ) == 0 ) //they are equivalent
						{
							ccOut.c = 0;
							ccOut.code = TC_HEIGHT_POP;
							return q - p;
						}
					} break;
				case 's':
				case 'S':
					{
						//compare "SIZE>" in all caps
						const char* q = p + 2;
						char buffer[16] = "";
						for(int i = 0; i < 5; i++)
						{
							buffer[i] = toupper( *(q + i) );
							if( buffer[i] == 0 )
								break;
						}

						//extract command close
						q += 5;
						if( strcmp( buffer, "SIZE>" ) == 0 ) //they are equivalent
						{
							ccOut.c = 0;
							ccOut.code = TC_SIZE_POP;
							return q - p;
						}
					} break;
				case 'a':
				case 'A':
					{
						//compare "ALIGN>" in all caps
						const char* q = p + 2;
						char buffer[16] = "";
						for(int i = 0; i < 6; i++)
						{
							buffer[i] = toupper( *(q + i) );
							if( buffer[i] == 0 )
								break;
						}

						//extract command close
						q += 6;
						if( strcmp( buffer, "ALIGN>" ) == 0 ) //they are equivalent
						{
							ccOut.c = 0;
							ccOut.code = TC_ALIGN_POP;
							return q - p;
						}
					} break;
				}
			} break;
		}
	}
	else if( *p == '\n' ) //new line
	{
		ccOut.c = 0;
		ccOut.code = TC_NEWLINE;
		return 1;
	}
	else if( *p == '\t' ) //tab
	{
		ccOut.c = 0;
		ccOut.code = TC_TAB;
		return 1;
	}
	else if( *p == ' ' ) //space
	{
		ccOut.c = 0;
		ccOut.code = TC_SPACE;
		return 1;
	}

	//just a character
	ccOut.c = *p;
	ccOut.code = TC_CHAR;
	return 1;
}

void CGuiText::serialize(std::string &strOut) const
{
	for(std::list<CharCode>::const_iterator i = m_text.begin(); i != m_text.end(); i++)
		codeToText( strOut, *i );
}

void CGuiText::codeToText(std::string &strOut, const CGuiText::CharCode &cc) const
{
	switch(cc.code)
	{
	case TC_CHAR:
		strOut += (char)cc.c;
		break;
	case TC_COLOR_PUSH:
		{
			strOut += "<COLOR=";
			char buffer[64] = "";
			strOut += itoa(cc.c, buffer, 16);
			strOut += '>';
		} break;
	case TC_COLOR_POP:
		strOut += "</COLOR>";
		break;
	case TC_WIDTH_PUSH:
		{
			strOut += "<WIDTH=";
			char buffer[64] = "";
			strOut += itoa(cc.c, buffer, 10);
			strOut += '>';
		} break;
	case TC_WIDTH_POP:
		strOut += "</WIDTH>";
		break;
	case TC_HEIGHT_PUSH:
		{
			strOut += "<HEIGHT=";
			char buffer[64] = "";
			strOut += itoa(cc.c, buffer, 10);
			strOut += '>';
		} break;
	case TC_HEIGHT_POP:
		strOut += "</HEIGHT>";
		break;
	case TC_SIZE_PUSH:
		{
			strOut += "<SIZE=";
			char buffer[64] = "";
			strOut += itoa(cc.c, buffer, 10);
			strOut += '>';
		} break;
	case TC_SIZE_POP:
		strOut += "</SIZE>";
		break;
	case TC_ALIGN_PUSH:
		{
			strOut += "<ALIGN=";
			switch( cc.c )
			{
			case ALIGN_LEFT:
				strOut += "LEFT";
				break;
			case ALIGN_CENTER:
				strOut += "CENTER";
				break;
			case ALIGN_RIGHT:
				strOut += "RIGHT";
				break;
			case ALIGN_JUSTIFIED:
				strOut += "JUSTIFIED";
			default:
				{
					char buffer[64] = "";
					strOut += "<<ERROR: UNKNOWN ALIGNMENT ";
					strOut += itoa(cc.c, buffer, 10);
					strOut += ">>";
				}
			}
			strOut += '>';
		} break;
	case TC_ALIGN_POP:
		strOut += "</ALIGN>";
		break;
	case TC_NEWLINE:
		strOut += '\n';
		break;
	case TC_TAB:
		strOut += '\t';
		break;
	case TC_SPACE:
		strOut += ' ';
		break;
	default:
		{
			strOut += "<<ERROR: UNKNOWN CODE ";
			char buffer[64];
			strOut += itoa(cc.code, buffer, 10);
			strOut += ">>";
		}
	}
}

bool CGuiText::newLine(unsigned int numPaintableOnLine, unsigned int maxLineWidth, unsigned int maxLineHeight)
{
	if( !m_isMultiline )
		return false;

	if(numPaintableOnLine > 0)
	{
		std::list<CharRegion>::iterator i = m_paintable.end();
		for(unsigned int j = 0; j < numPaintableOnLine; j++)
		{
			i--;
			i->region.bottom = i->region.top + maxLineHeight;

			//horizontal alignment
			long dw = 0;
			switch( m_hAlign.top() )
			{
			case ALIGN_LEFT:
				break;
			case ALIGN_CENTER:
				dw = (aabb().right - aabb().left - maxLineWidth) / 2;
				i->region.left += dw;
				i->region.right += dw;
				break;
			case ALIGN_RIGHT:
				dw = aabb().right - aabb().left - maxLineWidth;
				i->region.left += dw;
				i->region.right += dw;
				break;
			case ALIGN_JUSTIFIED: //not working yet
				break;
			}
		}
		numPaintableOnLine = 0;
		maxLineWidth = 0;
	}
	return true;
}

void CGuiText::updateRegions()
{
	if( m_color.empty() )
		m_color.push( m_defaultColor );
	if( m_width.empty() )
		m_width.push( m_defaultWidth );
	if( m_height.empty() )
		m_height.push( m_defaultHeight );
	if( m_hAlign.empty() )
		m_hAlign.push( m_defaultHAlign );

	m_paintable.clear(); //this is a terribly inefficient way to do this; I'm crying right now... :_(

	long hPosition = aabb().left, vPosition = aabb().top;
	unsigned int numPaintableOnLine = 0;
	unsigned int maxLineHeight = 0, maxLineWidth = 0;
	for(std::list<CharCode>::const_iterator i = m_text.begin(); i != m_text.end(); i++)
	{
		if( maxLineHeight < (unsigned int)abs(m_height.top()) )
			maxLineHeight = (unsigned int)abs(m_height.top());
		switch( i->code )
		{
		case TC_CHAR:
			{
				FontRect fr;
				GuiRect rgn = { hPosition, vPosition, hPosition + abs(m_width.top()), vPosition + abs(m_height.top()) };
				m_font->texCoord( &fr, i->c );
				CharRegion cr = { m_color.top(), m_width.top(), m_height.top(), rgn, fr };
				//need to check in case the letter goes past the edge, generate a newline possibly
				m_paintable.push_back( cr );

				hPosition += m_width.top();
				numPaintableOnLine++;
				maxLineWidth += abs( m_width.top() );
			} break;
		case TC_COLOR_PUSH:
			{
				if( m_color.size() < 65 ) //limit to 64 nested
					m_color.push( i->c );
			} break;
		case TC_COLOR_POP:
			{
				if(!m_color.empty())
					m_color.pop();
				if(m_color.empty())
					m_color.push( m_defaultColor );
			} break;
		case TC_WIDTH_PUSH:
			{
				if( m_width.size() < 65 ) //limit to 64 nested
					m_width.push( i->c );
			} break;
		case TC_WIDTH_POP:
			{
				if(!m_width.empty())
					m_width.pop();
				if(m_width.empty())
					m_width.push( m_defaultWidth );
			} break;
		case TC_HEIGHT_PUSH:
			{
				if( m_height.size() < 65 ) //limit to 64 nested
					m_height.push( i->c );
			} break;
		case TC_HEIGHT_POP:
			{
				if(!m_height.empty())
					m_height.pop();
				if(m_height.empty())
					m_height.push( m_defaultHeight );
			} break;
		case TC_SIZE_PUSH:
			{
				if( m_width.size() < 65 ) //limit to 64 nested
					m_width.push( i->c );
				if( m_height.size() < 65 ) //limit to 64 nested
					m_height.push( i->c );
			} break;
		case TC_SIZE_POP:
			{
				if(!m_width.empty())
					m_width.pop();
				if(m_width.empty())
					m_width.push( m_defaultWidth );
				if(!m_height.empty())
					m_height.pop();
				if(m_height.empty())
					m_height.push( m_defaultHeight );
			} break;
		case TC_ALIGN_PUSH:
			{
				if( maxLineWidth > 0 )
					if( !newLine( numPaintableOnLine, maxLineWidth, maxLineHeight ) )
						break;
					else
					{
						hPosition = aabb().left;
						vPosition += maxLineHeight;
						numPaintableOnLine = 0;
						maxLineWidth = 0;
						maxLineHeight = 0;
					}
				if( m_hAlign.size() < 65 ) //limit to 64 nested
					m_hAlign.push( i->c );
			} break;
		case TC_ALIGN_POP:
			{
				if( maxLineWidth > 0)
					if( !newLine( numPaintableOnLine, maxLineWidth, maxLineHeight ) )
						break;
					else
					{
						hPosition = aabb().left;
						vPosition += maxLineHeight;
						numPaintableOnLine = 0;
						maxLineWidth = 0;
						maxLineHeight = 0;
					}
				if(!m_hAlign.empty())
					m_hAlign.pop();
				if(m_hAlign.empty())
					m_hAlign.push( m_defaultHAlign );
			} break;
		case TC_NEWLINE:
			{
				if( newLine( numPaintableOnLine, maxLineWidth, maxLineHeight ) )
				{
					hPosition = aabb().left;
					vPosition += maxLineHeight;
					numPaintableOnLine = 0;
					maxLineWidth = 0;
					maxLineHeight = 0;
				}
			} break;
		case TC_TAB:
			{//should check for newline condition
				hPosition += abs(m_width.top()) * 4;
				maxLineWidth += abs(m_width.top()) * 4;
			} break;
		case TC_SPACE:
			{//should check for newline condition
				hPosition += abs(m_width.top());
				maxLineWidth += abs(m_width.top());
			} break;
		}
	}

	if(numPaintableOnLine > 0)
	{
		std::list<CharRegion>::iterator i = m_paintable.end();
		for(unsigned int j = 0; j < numPaintableOnLine; j++)
		{
			i--;
			i->region.bottom = i->region.top + maxLineHeight;

			//horizontal alignment applied
			long dw = 0;
			switch( m_hAlign.top() )
			{
			case ALIGN_LEFT:
				break;
			case ALIGN_CENTER:
				dw = (aabb().right - aabb().left - maxLineWidth) / 2;
				i->region.left += dw;
				i->region.right += dw;
				break;
			case ALIGN_RIGHT:
				dw = aabb().right - aabb().left - maxLineWidth;
				i->region.left += dw;
				i->region.right += dw;
				break;
			case ALIGN_JUSTIFIED: //not working yet
				break;
			}
		}
		vPosition += maxLineHeight;
		hPosition = aabb().left;
		numPaintableOnLine = 0;
		maxLineHeight = 0;
		maxLineWidth = 0;
	}

	for(std::list<CharRegion>::iterator i = m_paintable.begin(); i != m_paintable.end(); i++)
	{
		//vertical alignment applied
		long dh = 0;
		switch( m_defaultVAlign )
		{
		case ALIGN_TOP:
			break;
		case ALIGN_MIDDLE:
			dh = ( aabb().bottom - aabb().top - vPosition ) / 2;
			i->region.top += dh;
			i->region.bottom += dh;
			break;
		case ALIGN_BOTTOM:
			dh = aabb().bottom - aabb().top - vPosition;
			i->region.top += dh;
			i->region.bottom += dh;
			break;
		case ALIGN_JUSTIFIED: //not working yet
			break;
		}

		//finalize this paintable char region
		i->finalize();
	}

	//empty the stacks to prevent crashing by string codes
	while( !m_color.empty() )
		m_color.pop();
	while( !m_width.empty() )
		m_width.pop();
	while( !m_height.empty() )
		m_height.pop();
	while( !m_hAlign.empty() )
		m_hAlign.pop();

	//if there is <= 256 letters, update the vertex buffer
	if( m_paintable.size() <= 256 )
	{
		unsigned int vIndex = 0;
		for(std::list<CharRegion>::const_iterator i = m_paintable.begin(); i != m_paintable.end(); i++)
		{
			memcpy( (m_vb->pData.vertex + vIndex), i->v, 4 * sizeof(VertexData) ); //copies all 4 vertices from the char region
			vIndex += 4;
		}
		graphic()->vertexUpdate(0, vIndex, m_vb);
	}
}

int CGuiText::proc(unsigned int msg, unsigned int param1, void *param2)
{
	switch(msg)
	{
	case GUI_HITTEST:
		{
			if(m_isSelectable)
				return CGuiObject::proc( msg, param1, param2 );
			return 0;
		} break;
	case GUI_CREATE:
		{
			CGuiObject::proc( msg, param1, param2 );
			GuiCreationParams* cp = reinterpret_cast<GuiCreationParams*>(param2);
			if(cp->pExtra != 0)
			{
				GuiTextCreate* tc = reinterpret_cast<GuiTextCreate*>(cp->pExtra);
				m_defaultColor = tc->defaultColor;
				m_defaultWidth = tc->defaultWidth;
				m_defaultHeight = tc->defaultHeight;
				if( m_defaultHeight == 0 )
					m_defaultHeight = m_defaultWidth;
			}
			else
			{
				m_defaultColor = 0xffffffff;
				m_defaultWidth = 30;
				m_defaultHeight = 30;
			}
			if( cp->dwFlags & GTS_ALLOW_TEXT_CODES )
				m_allowsGuiTextCode = true;
			if( cp->dwFlags & GTS_SELECTABLE )
				m_isSelectable = true;
			if( cp->dwFlags & GTS_SHOW_CARET )
				m_allowsCaret = true;
			if( cp->dwFlags & GTS_MULTILINE )
				m_isMultiline = true;
			if( cp->dwFlags & GTS_AUTOWRAP )
				m_isAutoWrap = true;
			if( cp->dwFlags & GTS_CLIP_TEXT )
				m_isClipping = true;

			m_defaultVAlign = ALIGN_TOP;
			if( cp->dwFlags & GTS_VALIGN_MIDDLE )
				m_defaultVAlign = ALIGN_MIDDLE;
			if( cp->dwFlags & GTS_VALIGN_BOTTOM )
				m_defaultVAlign = ALIGN_BOTTOM;

			m_defaultHAlign = ALIGN_LEFT;
			if( cp->dwFlags & GTS_HALIGN_CENTER )
				m_defaultHAlign = ALIGN_CENTER;
			if( cp->dwFlags & GTS_HALIGN_RIGHT )
				m_defaultHAlign = ALIGN_RIGHT;

			graphic()->vertexCreate( 256*4, GRU_dynamic, 0, &m_vb );

			extract( text().c_str() );
			if( m_allowsCaret )
				manager()->timeKeeper()->addCallback( &m_caretFlash );
		} break;
	case GUI_DESTROY:
		{
			manager()->timeKeeper()->removeCallback( &m_caretFlash );
			graphic()->vertexDestroy( m_vb );
			m_vb = 0;
			m_text.clear();

			return CGuiObject::proc( msg, param1, param2 );
		} break;
	case GUI_PAINT:
		{
			preparePaint();

			memcpy( m_cb->pData.cb, param2, sizeof(Matrix) );
			graphic()->setConstantBuffer( m_cb );
			graphic()->setIndexBuffer( m_ib );
			graphic()->setVertexBuffer( m_vb );
			graphic()->setShader( S_simpleTexture );
			graphic()->setTexture( m_font->texture(), 0 );

			if( m_paintable.size() > 256 )
			{
				unsigned int vIndex = 0;
				for(std::list<CharRegion>::const_iterator i = m_paintable.begin(); i != m_paintable.end(); i++)
				{
					memcpy( (m_vb->pData.vertex + vIndex), i->v, 4 * sizeof(VertexData) ); //copies all 4 vertices from the char region
					vIndex += 4;
					if( vIndex == m_vb->length ) //if it reaches max length, render
					{
						graphic()->vertexUpdate(0, 0, m_vb);
						graphic()->render();
						vIndex = 0;
					}
				}
				if( vIndex != m_vb->length ) //it didn't render this set yet
				{
					graphic()->vertexUpdate(0, vIndex, m_vb);
					graphic()->render(0, 0, vIndex / 4 * 6);
				}
			}
			else
			{
				graphic()->render(0, 0, m_paintable.size() * 6);
			}

			return CGuiObject::proc( msg, param1, param2 );
		} break;
	case GUI_MOVE_FROM_PARENT:
		{
			CGuiObject::proc( msg, param1, param2 );
			updateRegions();
		} break;
	case GUI_SIZE:
		{
			CGuiObject::proc( msg, param1, param2 );
			updateRegions();
		} break;
	case GUI_IS_MOVABLE_BY_MOUSE:
		{
			return 0; //not movable by mouse
		} break;
	case GUI_SET_TEXT:
		{
			CGuiObject::proc( msg, param1, param2 );
			m_text.clear();
			extract( text().c_str() );
		} break;
	case GUI_GET_TEXT:
		{
			std::string strText = "";
			serialize( strText );
			unsigned int nCopied = strText.size();
			if(nCopied > param1)
				nCopied = param1;
			memcpy( param2, strText.c_str(), nCopied );
			return (int)nCopied;
		} break;
	default:
		return CGuiObject::proc( msg, param1, param2 );
	}

	return 0;
}










CGuiTextFactory::CGuiTextFactory()
: m_manager(0), m_ib(0), m_cb(0)
{
}

CGuiTextFactory::~CGuiTextFactory()
{
	m_control.clear();

	if(m_manager != 0)
	{
		m_manager->graphic()->indexDestroy( m_ib );
		m_ib = 0;
		m_manager->graphic()->constantDestroy( m_cb );
		m_cb = 0;
	}

	m_manager = 0;
}

GuiObject* CGuiTextFactory::createInstance()
{
	m_control.push_back( CGuiText(&m_font, m_cb, m_ib) );
	return (GuiObject*)&m_control.back();
}

void CGuiTextFactory::destroyInstance(GuiObject *pObject)
{
	if(pObject == 0)
		return;
	for(std::list<CGuiText>::iterator i = m_control.begin(); i != m_control.end(); i++)
	{
		if( pObject == (GuiObject*)(&(*i)) )
		{
			m_control.erase( i );
			break;
		}
	}
}

void CGuiTextFactory::notifyRegister(GuiManager *pManager)
{
	m_manager = pManager;
	if(m_manager == 0)
		return;

	IndexData id[256*6];
	for(int i = 0, v = 0; i < 256*6; i+=6, v+=4)
	{
		id[i+0] = v+0;
		id[i+1] = v+1;
		id[i+2] = v+2;
		id[i+3] = v+2;
		id[i+4] = v+3;
		id[i+5] = v+0;
	}
	m_manager->graphic()->indexCreate( 256*6, GRU_immutable, id, &m_ib );
	m_manager->graphic()->constantCreate( SIR_wvp, 0, &m_cb );

	m_font.initialize( m_manager->graphic(), "textures\\font.png" );
}

void CGuiTextFactory::notifyUnregister()
{
	m_control.clear();

	if(m_manager != 0)
	{
		m_manager->graphic()->indexDestroy( m_ib );
		m_ib = 0;
		m_manager->graphic()->constantDestroy( m_cb );
		m_cb = 0;
	}

	m_font.shutdown();

	m_manager = 0;
}

const char* CGuiTextFactory::szClass()
{
	return "text";
}
