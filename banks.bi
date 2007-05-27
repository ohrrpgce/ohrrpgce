type voice
	vbyte(0 to 10) as ubyte
end type

dim shared gm_name(0 to 127) as string * 8 => { _
    "ACGPIANO", _
    "ACPIANO", _
    "ELGPIANO", _
    "HONKTONK", _
    "ELPIANO1", _
    "ELPIANO2", _
    "HARPSCHD", _
    "CLAVICHD", _
    "CELESTA", _
    "GLOCK", _
    "MUSICBOX", _
    "VIBES", _
    "MARIMBA", _
    "XYLOPHON", _
    "TUBEBELL", _
    "PIANOBEL", _
    "BARORGAN", _
    "PRCORGAN", _
    "ROCKORGN", _
    "PIPEORGN", _
    "REEDORGN", _
    "ACCORDN", _
    "HARMNICA", _
    "TANGO", _
    "NYLON", _
    "ACOUST", _
    "JAZZGTR", _
    "ELGUITAR", _
    "ELGMUTE", _
    "OVERDRIV", _
    "HVYMETAL", _
    "GUITHARM", _
    "ACOUBASS", _
    "ELECBASS", _
    "PICKBASS", _
    "FRETLESS", _
    "SLAPBASS", _
    "SLAPBAS2", _
    "SYNBASS1", _
    "SYNBASS2", _
    "VIOLIN", _
    "VIOLA", _
    "CELLO", _
    "DBLBASS", _
    "STRINGS1", _
    "PIZZ", _
    "HARP", _
    "TIMPANI", _
    "EXCELSTR", _
    "STRSECT", _
    "SYNSTR1", _
    "SYNSTR2", _
    "AAHs", _
    "OOHs", _
    "VOXSYNTH", _
    "ORCHHIT", _
    "TRUMPET1", _
    "TROMB1", _
    "TUBA3", _
    "SOFTRUMP", _
    "FRHORN1", _
    "BRASSECT", _
    "SYNBRASS", _
    "BRASSOFT", _
    "SOPRANO", _
    "ALTOSAX", _
    "TENORSAX", _
    "BARISAX", _
    "OBOE", _
    "ENGLHORN", _
    "BASSOON", _
    "CLARINET", _
    "PICCOLO", _
    "FLUTE", _
    "RECORDER", _
    "PANPIPES", _
    "BOTTLE", _
    "SHAKAKU", _
    "WHISTLE", _
    "WOOD", _
    "LEAD1", _
    "LEAD2", _
    "LEAD3", _
    "LEAD4", _
    "LEAD5", _
    "WUZZLE", _
    "LEAD7", _
    "SYNBASS1", _
    "NEWAGE", _
    "WARMPAD", _
    "PAD3", _
    "PAD4", _
    "PAD5", _
    "PAD6", _
    "PAD7", _
    "PAD8", _
    "FX1", _
    "FX2", _
    "FX3", _
    "FX4 Atms", _
    "FX5", _
    "FX6", _
    "FX7", _
    "FX8", _
    "SITAR", _
    "BANJO", _
    "SHAMISEN", _
    "KOTO", _
    "KALIMBA", _
    "BAGPIPE", _
    "FIDDLE", _
    "ETHNIC", _
    "HANDBELL", _
    "AGOGO", _
    "STEELDRM", _
    "WOODBLOC", _
    "TAIDRUM", _
    "MELDRUM", _
    "SYNSNAR", _
    "REVCYMB", _
    "CHICKEN", _
    "BREATH", _
    "SEASHORE", _
    "INSECTS", _
    "OINKEY", _
    "HELICPTR", _
    "CROWD", _
    "SHOTGUN" _
}

dim shared gm_voice(0 to 127, 0 to 10) as ubyte => { _
    { &h21, &h31, &h4F, &h80, &hF2, &h72, &h52, &h73, &h0, &h0, &h6 }, _
    { &h1, &h1, &h4B, &h83, &hF2, &h92, &h50, &h76, &h0, &h0, &h6 }, _
    { &h13, &h11, &hC6, &h5, &hF2, &hF1, &hF5, &hF5, &h1, &h0, &h0 }, _
    { &h81, &h41, &h12, &h0, &hF2, &hF2, &hF7, &hF7, &h0, &h0, &h36 }, _
    { &h1, &h1, &h57, &h0, &hF1, &hF2, &hF7, &hF7, &h0, &h0, &h30 }, _
    { &h1, &h1, &h93, &h0, &hF1, &hF2, &hF7, &hF7, &h0, &h0, &h0 }, _
    { &h31, &h32, &h8E, &h80, &hF1, &hF3, &hF9, &hF9, &h0, &h0, &hA }, _
    { &h1, &h11, &h49, &h2, &hF1, &hF1, &h53, &h74, &h1, &h2, &h6 }, _
    { &hC, &h81, &h5C, &h0, &hF6, &hF3, &hF4, &hF5, &h0, &h0, &h30 }, _
    { &h7, &h11, &h97, &h80, &hF3, &hF2, &hF2, &hF1, &h0, &h0, &h32 }, _
    { &h17, &h1, &h21, &h0, &h54, &hF4, &hF4, &hF4, &h0, &h0, &h32 }, _
    { &h93, &h91, &h97, &h0, &hAA, &hAC, &h12, &h21, &h2, &h0, &hE }, _
    { &h18, &h1, &h23, &h0, &hF6, &hE7, &hF6, &hF7, &h0, &h0, &h30 }, _
    { &h15, &h1, &h91, &h0, &hF6, &hF6, &hF6, &hF6, &h0, &h0, &h34 }, _
    { &h13, &h1, &h4F, &h10, &hF2, &hF2, &h60, &h72, &h0, &h0, &h8 }, _
    { &h3, &h81, &h49, &h80, &h75, &hB5, &hF5, &hF5, &h1, &h0, &h34 }, _
    { &h64, &h21, &h86, &h80, &hFF, &hFF, &hF, &hF, &h0, &h0, &h1 }, _
    { &h72, &h30, &h14, &h0, &hC7, &hC7, &h58, &h8, &h0, &h0, &h32 }, _
    { &h70, &hB1, &h44, &h0, &hAA, &h8A, &h18, &h8, &h0, &h0, &h34 }, _
    { &hB2, &hB0, &h43, &h0, &h9F, &h95, &h6, &hF, &h4, &h1, &h9 }, _
    { &h61, &hB1, &h13, &h80, &h97, &h55, &h4, &h4, &h1, &h0, &h30 }, _
    { &h24, &hB1, &h48, &h0, &h98, &h46, &h2A, &h1A, &h1, &h0, &h3C }, _
    { &h61, &h21, &h13, &h0, &h91, &h61, &h6, &h7, &h1, &h0, &h3A }, _
    { &hE1, &hE1, &hD0, &h0, &hF5, &hF4, &hAF, &hF, &h0, &h1, &hC }, _
    { &h13, &h11, &h96, &h80, &hFF, &hFF, &h21, &h3, &h0, &h0, &hA }, _
    { &h11, &h11, &h8D, &h80, &hFF, &hFF, &h1, &h3, &h0, &h0, &h0 }, _
    { &h3, &h11, &h5E, &h0, &hF5, &hF2, &h71, &h83, &h1, &h0, &hE }, _
    { &h3, &h21, &h87, &h80, &hF6, &hF3, &h22, &hF8, &h1, &h0, &h36 }, _
    { &h1, &h1, &h11, &h0, &hF2, &hF5, &h1F, &h88, &h0, &h0, &hA }, _
    { &h31, &h32, &h48, &h0, &hF1, &hF2, &h53, &h27, &h0, &h2, &h6 }, _
    { &h61, &hE6, &h40, &h3, &h91, &hC1, &h1A, &h1A, &h0, &h0, &h8 }, _
    { &hC8, &hC4, &h12, &h3, &h73, &hF4, &hBF, &h9F, &h0, &h0, &h8 }, _
    { &h1, &h0, &h0, &h0, &h94, &h83, &hB6, &h26, &h0, &h0, &h1 }, _
    { &h1, &h2, &h62, &h0, &hC1, &hF3, &hEE, &hDE, &h0, &h0, &hA }, _
    { &h31, &h31, &h8D, &h0, &hF1, &hF1, &hE8, &h78, &h0, &h0, &h3A }, _
    { &h31, &h22, &h1E, &h0, &hF2, &hF5, &hEF, &h78, &h0, &h0, &hE }, _
    { &h31, &h23, &hB, &h0, &h72, &hD5, &hB5, &h98, &h1, &h0, &h8 }, _
    { &h31, &h22, &h10, &h4, &h83, &hF4, &h9F, &h78, &h0, &h0, &hA }, _
    { &h11, &h31, &h5, &h0, &hF9, &hF1, &h25, &h34, &h0, &h0, &hA }, _
    { &h1, &h11, &hF, &h0, &hD5, &h85, &h24, &h22, &h0, &h0, &hA }, _
    { &h30, &hA0, &h1C, &h0, &h51, &h53, &h3, &h47, &h2, &h2, &hE }, _
    { &h31, &h21, &h16, &h0, &hDD, &h66, &h13, &h6, &h1, &h0, &h38 }, _
    { &hB1, &hB2, &hC3, &h80, &h52, &h41, &h11, &hFE, &h1, &h1, &h0 }, _
    { &h21, &h23, &h4D, &h80, &h71, &h72, &h12, &h6, &h1, &h0, &h2 }, _
    { &hF1, &hE1, &h40, &h0, &hF1, &h4F, &h21, &h16, &h1, &h0, &h2 }, _
    { &h2, &h1, &h1A, &h80, &hF5, &h85, &h75, &h35, &h1, &h0, &h30 }, _
    { &h2, &h1, &h1D, &h80, &hF5, &hF3, &h75, &hF4, &h1, &h0, &h30 }, _
    { &h10, &h11, &h41, &h0, &hF5, &hF2, &h5, &hC3, &h1, &h0, &h32 }, _
    { &h21, &hA2, &h9B, &h1, &hB1, &h72, &h25, &h8, &h1, &h0, &h3E }, _
    { &hB1, &h61, &h8B, &h40, &h51, &h42, &h11, &h15, &h0, &h1, &h6 }, _
    { &h31, &h62, &h1A, &h40, &h75, &h54, &h3, &h44, &h1, &h0, &hE }, _
    { &h21, &h22, &h16, &h7, &h70, &h73, &h81, &h2C, &h1, &h1, &hC }, _
    { &h31, &h72, &h5B, &h83, &hF4, &h8A, &h15, &h5, &h0, &h0, &h30 }, _
    { &h61, &hE1, &hA7, &h81, &h72, &h50, &h8B, &h19, &h0, &h0, &h2 }, _
    { &h71, &h72, &h57, &h0, &h54, &h7A, &h5, &h5, &h0, &h0, &h3C }, _
    { &hC1, &hC1, &hF, &h0, &h91, &h62, &h6, &h5, &h1, &h2, &hC }, _
    { &h31, &h61, &h1C, &h0, &h41, &h92, &hB, &hB, &h0, &h0, &hE }, _
    { &h31, &h61, &h1E, &h0, &h41, &h82, &h1F, &hB, &h0, &h0, &hE }, _
    { &h0, &h1, &h1D, &h0, &h52, &h73, &h65, &h76, &h0, &h0, &hE }, _
    { &h21, &h21, &h19, &h80, &h43, &h85, &h8C, &h2F, &h0, &h0, &hC }, _
    { &h21, &h21, &h9B, &h0, &h61, &h7F, &h6A, &hA, &h0, &h0, &h32 }, _
    { &h61, &h22, &h8A, &h6, &h75, &h74, &h1F, &hF, &h0, &h0, &h38 }, _
    { &h21, &h21, &h8E, &h80, &hBB, &h90, &h29, &hA, &h0, &h0, &h8 }, _
    { &hE1, &hE1, &h16, &hA, &h71, &h81, &hAE, &h9E, &h0, &h0, &hA }, _
    { &hE0, &hE2, &h23, &h0, &h71, &h80, &hAE, &h9E, &h2, &h1, &hA }, _
    { &h1, &h12, &h4F, &h0, &h71, &h52, &h53, &h7C, &h0, &h0, &hA }, _
    { &h21, &h32, &h4E, &h0, &h71, &h52, &h68, &h5E, &h0, &h0, &hA }, _
    { &h11, &h12, &h56, &h0, &h71, &h52, &h5B, &h7B, &h0, &h0, &hE }, _
    { &h21, &h24, &h94, &h5, &hF0, &h90, &h9, &hA, &h0, &h0, &hA }, _
    { &hB1, &hA1, &hC5, &h80, &h6E, &h8B, &h17, &hE, &h0, &h0, &h2 }, _
    { &h31, &h32, &hD1, &h80, &hD5, &h61, &h19, &h1B, &h0, &h0, &hC }, _
    { &h32, &h61, &h9A, &h82, &h51, &hA2, &h1B, &h3B, &h0, &h0, &hC }, _
    { &hA1, &hA1, &h1D, &h85, &h95, &h60, &h24, &h2A, &h0, &h0, &h2 }, _
    { &h21, &hA2, &h83, &h8D, &h74, &h65, &h17, &h17, &h0, &h0, &h7 }, _
    { &hA1, &h21, &h9C, &h0, &h75, &h75, &h1F, &hA, &h0, &h0, &h2 }, _
    { &hA2, &hA1, &h12, &h8B, &hF5, &h61, &h30, &h3A, &h0, &h0, &h2 }, _
    { &hE2, &h61, &h6D, &h0, &h57, &h57, &h4, &h7, &h0, &h0, &hE }, _
    { &hF1, &hE1, &h28, &h0, &h57, &h67, &h34, &h5D, &h3, &h0, &hE }, _
    { &hF1, &hF1, &h1A, &h0, &hC, &h60, &hC7, &hA5, &h0, &h0, &hD }, _
    { &h62, &hA1, &h93, &h0, &h77, &h76, &h7, &h7, &h0, &h0, &h3B }, _
    { &h4, &h1, &h8, &h5, &hF8, &h82, &h7, &h74, &h0, &h0, &h8 }, _
    { &h60, &h60, &h3, &h4, &hF6, &h76, &h4F, &hF, &h0, &h0, &h2 }, _
    { &h82, &hF1, &hD, &h0, &h97, &h97, &h8, &h8, &h0, &h0, &h0 }, _
    { &h51, &h1, &h80, &h0, &h55, &h55, &hF5, &hF5, &h0, &h0, &h8 }, _
    { &h61, &h21, &h0, &h2, &h96, &h55, &h33, &h2B, &h0, &h0, &h6 }, _
    { &h51, &h41, &hD, &h0, &hF2, &hF2, &hF2, &hF2, &h0, &h2, &hA }, _
    { &h11, &h3, &h80, &h80, &hA3, &hA1, &hE1, &hE4, &h0, &h0, &h6 }, _
    { &h11, &h31, &h5, &h0, &hF9, &hF1, &h25, &h34, &h0, &h0, &hA }, _
    { &h71, &h23, &h0, &h0, &hF1, &hF4, &h45, &h44, &h1, &h0, &h5 }, _
    { &hE0, &hF0, &h16, &h3, &hB1, &hE0, &h51, &h75, &h2, &h2, &h0 }, _
    { &h51, &h1, &h3, &h8, &hFF, &hFF, &h2, &h2, &h1, &h0, &h4 }, _
    { &hE1, &hE1, &hD0, &h0, &hF5, &hF4, &hAF, &hF, &h0, &h1, &hC }, _
    { &hF1, &hF1, &h46, &h80, &h22, &h31, &h11, &h2E, &h1, &h0, &hC }, _
    { &h5, &h46, &h40, &h80, &hB3, &hF2, &hD3, &h24, &h0, &h0, &h2 }, _
    { &h1, &h11, &hD, &h80, &hF1, &h50, &hFF, &hFF, &h0, &h0, &h6 }, _
    { &h0, &h11, &h12, &h80, &h10, &h50, &hFF, &hFF, &h0, &h0, &hA }, _
    { &hB4, &hF5, &h87, &h80, &hA4, &h45, &h2, &h42, &h0, &h0, &h6 }, _
    { &hF1, &hF1, &h41, &h41, &h11, &h11, &h11, &h11, &h0, &h0, &h2 }, _
    { &hB4, &hF7, &h87, &h80, &hA4, &h45, &h2, &h42, &h0, &h0, &h6 }, _
    { &h61, &h60, &h54, &h3, &h78, &hA2, &h0, &h47, &h1, &h2, &h6 }, _
    { &h40, &h8, &hD, &h0, &hFF, &hFF, &h3, &h1, &h0, &h0, &h8 }, _
    { &h0, &h0, &h0, &h0, &h5F, &hFF, &hF, &h5, &h0, &h0, &h0 }, _
    { &h21, &h21, &h56, &h0, &h7F, &h35, &h41, &h21, &h0, &h0, &hE }, _
    { &h71, &h31, &h0, &h40, &hF1, &hF1, &h1, &h1, &h3, &h0, &h4 }, _
    { &h2, &h7, &h85, &h3, &hD2, &hF2, &h53, &hF6, &h0, &h1, &h30 }, _
    { &h31, &h16, &h87, &h80, &hA1, &h7D, &h11, &h43, &h0, &h0, &h8 }, _
    { &h1, &h19, &h4F, &h0, &hF1, &hF2, &h53, &h74, &h0, &h0, &h6 }, _
    { &h93, &h91, &h91, &h0, &hD4, &hEB, &h32, &h11, &h0, &h1, &h38 }, _
    { &h2, &h1, &h99, &h80, &hF5, &hF6, &h55, &h53, &h0, &h0, &h0 }, _
    { &h31, &h22, &h43, &h5, &h6E, &h8B, &h17, &hC, &h1, &h2, &h2 }, _
    { &h31, &h62, &h1C, &h0, &h75, &h54, &h3, &h44, &h1, &h0, &hE }, _
    { &h80, &h95, &h4D, &h0, &h78, &h85, &h42, &h54, &h0, &h0, &hE }, _
    { &h7, &h8, &h48, &h80, &hF1, &hFC, &h72, &h4, &h0, &h0, &h0 }, _
    { &h7, &h2, &h15, &h0, &hEC, &hF8, &h26, &h16, &h0, &h0, &h3A }, _
    { &h2, &h0, &hC0, &h0, &h8F, &hFF, &h6, &h5, &h1, &h0, &hA }, _
    { &h2, &h2, &h0, &h0, &hC8, &hC8, &h97, &h97, &h0, &h0, &h1 }, _
    { &h1, &h1, &h0, &h0, &hFF, &hFF, &h7, &h7, &h0, &h0, &h7 }, _
    { &h11, &h10, &h41, &h3, &hF8, &hF3, &h47, &h3, &h2, &h0, &h34 }, _
    { &h6, &h0, &h0, &h0, &hF0, &hF6, &hF0, &hB5, &h0, &h0, &hE }, _
    { &hE, &hE0, &hA, &hA, &h1F, &h11, &h0, &hF5, &h0, &h3, &hE }, _
    { &h51, &h42, &h0, &h5, &h66, &h66, &h5, &h6, &h2, &h0, &h0 }, _
    { &h53, &h0, &h5, &h0, &h5F, &h7F, &h66, &h7, &h0, &h0, &h6 }, _
    { &hE, &hC0, &h0, &h8, &hF6, &h1F, &h0, &h2, &h0, &h3, &hE }, _
    { &hC0, &h7E, &h4F, &hC, &hF1, &h20, &h3, &h2, &h0, &h0, &h2 }, _
    { &hF4, &hF3, &h50, &h80, &h85, &h74, &h87, &h99, &h0, &h0, &hC }, _
    { &hF0, &hE2, &h0, &hC0, &h1E, &h21, &h11, &h11, &h1, &h1, &h8 }, _
    { &h7E, &h6E, &h0, &h0, &hFF, &h3F, &hF, &hF, &h0, &h0, &hE }, _
    { &h6, &h84, &h0, &h0, &hA0, &hC6, &hF0, &h75, &h0, &h0, &hE } _
}

dim shared ibank_name(0 to 127) as string * 8 => { _
    "ACOUPNO1", _
    "ACOUPNO2", _
    "ACOUPNO3", _
    "ELECPNO1", _
    "ELECPNO2", _
    "ELECPNO3", _
    "ELECPNO4", _
    "HONKTONK", _
    "ELECORG1", _
    "ELECORG2", _
    "ELECORG3", _
    "ELECORG4", _
    "PIPEORG1", _
    "PIPEORG2", _
    "PIPEORG3", _
    "ACCORDN", _
    "HARPSI1", _
    "HARPSI2", _
    "HARPSI3", _
    "CLAVI1", _
    "CLAVI2", _
    "CLAVI3", _
    "CELESTA1", _
    "CELESTA2", _
    "SYNBRSS1", _
    "SYNBRSS2", _
    "SYNBRSS3", _
    "SYNBRSS4", _
    "SYNBASS1", _
    "SYNBASS2", _
    "SYNBASS3", _
    "SYNBASS4", _
    "FANTASY", _
    "HARMOPAN", _
    "CHORALE", _
    "GLASSES", _
    "SOUNDTRK", _
    "ATMOSPHR", _
    "WARMBELL", _
    "FUNNYVOX", _
    "ECHOBELL", _
    "ICERAIN", _
    "OBOE2001", _
    "ECHOPAN", _
    "DOCSOLO", _
    "SCHLDAZE", _
    "BELLSING", _
    "SQWAVE", _
    "STRSECT1", _
    "STRSECT2", _
    "STRSECT3", _
    "PIZZICTO", _
    "VIOLIN1", _
    "VIOLIN2", _
    "CELLO1", _
    "CELLO2", _
    "CONTRABS", _
    "HARP1", _
    "HARP2", _
    "GUITAR1", _
    "GUITAR2", _
    "ELECGTR1", _
    "ELECGTR2", _
    "SITAR", _
    "ACOUBS1", _
    "ACOUBS2", _
    "ELECBS1", _
    "ELECBS2", _
    "SLAPBS1", _
    "SLAPBS2", _
    "FRETLS1", _
    "FRETLS2", _
    "FLUTE1", _
    "FLUTE2", _
    "PICCOLO1", _
    "PICCOLO2", _
    "RECORDER", _
    "PANPIPES", _
    "SAX1", _
    "SAX2", _
    "SAX3", _
    "SAX4", _
    "CLARINT1", _
    "CLARINT2", _
    "OBOE", _
    "ENGHORN", _
    "BASSOON", _
    "HARMONCA", _
    "TRUMPET1", _
    "TRUMPET2", _
    "TROMBON1", _
    "TROMBON2", _
    "FRHORN1", _
    "FRHORN2", _
    "TUBA", _
    "BRSSECT1", _
    "BRSSECT2", _
    "VIBES1", _
    "VIBES2", _
    "SYNMALLT", _
    "WINDBELL", _
    "GLOCK", _
    "TUBEBELL", _
    "XYLOPHON", _
    "MARIMBA", _
    "KOTO", _
    "SHO", _
    "SHAKUHCH", _
    "WHISTLE1", _
    "WHISTLE2", _
    "BOTTLBLO", _
    "BREATHPP", _
    "TIMPANI", _
    "MELODTOM", _
    "DEEPSNAR", _
    "OBERHEIM", _
    "NOISE", _
    "TAIKO", _
    "TAIKORIM", _
    "REVCYMB", _
    "JAWHARP", _
    "TRIANGLE", _
    "ORCHEHIT", _
    "BASSDRM", _
    "BIRDTWT", _
    "BANJO", _
    "MOOGSYN", _
    "JUNGLTUN" _
}

dim shared ibank_voice(0 to 127, 0 to 10) as ubyte => { _
    { &h2, &h1, &h50, &hE, &hF1, &hD2, &h50, &h76, &h0, &h0, &h6 }, _
    { &h2, &h1, &h50, &hE, &hF1, &hD2, &h50, &h76, &h0, &h0, &h6 }, _
    { &h1, &h1, &h4B, &hE, &hF1, &hD2, &h50, &h76, &h0, &h0, &h6 }, _
    { &h13, &h1, &h50, &hE, &hF1, &hD2, &h50, &h76, &h0, &h0, &h6 }, _
    { &h32, &h1, &h92, &h8B, &hFF, &hFF, &h11, &h13, &h0, &h0, &hA }, _
    { &h34, &h3, &h92, &hB, &hFF, &hFF, &h10, &h4, &h0, &h0, &hA }, _
    { &h34, &h3, &h92, &hB, &hFF, &hFF, &h10, &h4, &h0, &h0, &hA }, _
    { &h53, &h51, &h4E, &hB, &hF1, &hD2, &h0, &h86, &h0, &h0, &h6 }, _
    { &h28, &h21, &hCF, &hB, &hF8, &hC0, &hE5, &hFF, &h0, &h0, &h0 }, _
    { &hE2, &hE1, &hCA, &hB, &hF8, &hC0, &hE5, &hE, &h0, &h0, &h8 }, _
    { &h2C, &hA1, &hD4, &hB, &hF9, &hC0, &hFF, &hFF, &h0, &h0, &h0 }, _
    { &h2B, &h21, &hCA, &hB, &hF8, &hC0, &hE5, &hFF, &h0, &h0, &h0 }, _
    { &h29, &h21, &hCD, &hB, &hF0, &hE0, &h91, &h86, &h0, &h0, &h2 }, _
    { &h24, &h21, &hD0, &hB, &hF0, &hE0, &h1, &h86, &h0, &h0, &h2 }, _
    { &h23, &h21, &hC8, &hB, &hF0, &hE0, &h1, &h86, &h0, &h0, &h2 }, _
    { &h64, &h61, &hC9, &hB, &hB0, &hF0, &h1, &h86, &h0, &h0, &h2 }, _
    { &h33, &h15, &h85, &h8B, &hA1, &h72, &h10, &h23, &h0, &h0, &h8 }, _
    { &h31, &h15, &h85, &h8B, &hA1, &h73, &h10, &h33, &h0, &h0, &h8 }, _
    { &h31, &h16, &h81, &h8B, &hA1, &hC2, &h30, &h74, &h0, &h0, &h8 }, _
    { &h3, &h2, &h8A, &h8B, &hF0, &hF4, &h7B, &h7B, &h0, &h0, &h8 }, _
    { &h3, &h1, &h8A, &h80, &hF0, &hF4, &h7B, &h7B, &h0, &h0, &h8 }, _
    { &h23, &h1, &h8A, &h80, &hF2, &hF4, &h7B, &h7B, &h0, &h0, &h8 }, _
    { &h32, &h12, &h80, &h8B, &h1, &h72, &h10, &h33, &h0, &h0, &h8 }, _
    { &h32, &h14, &h80, &h8B, &h1, &h73, &h10, &h33, &h0, &h0, &h8 }, _
    { &h31, &h21, &h16, &hB, &h73, &h80, &h8E, &h9E, &h0, &h0, &hE }, _
    { &h30, &h21, &h16, &hB, &h73, &h80, &h7E, &h9E, &h0, &h0, &hE }, _
    { &h31, &h21, &h94, &h0, &h33, &hA0, &h73, &h97, &h0, &h0, &hE }, _
    { &h31, &h21, &h94, &hC, &hD3, &hA0, &h73, &h97, &h0, &h0, &hE }, _
    { &h31, &h32, &h45, &hB, &hF1, &hF2, &h53, &h27, &h0, &h0, &h6 }, _
    { &h13, &h15, &hC, &h18, &hF2, &hF2, &h1, &hB6, &h0, &h0, &h8 }, _
    { &h11, &h11, &hC, &hB, &hF2, &hF2, &h1, &hB6, &h0, &h0, &h8 }, _
    { &h11, &h11, &hA, &hB, &hFE, &hF2, &h4, &hBD, &h0, &h0, &h8 }, _
    { &h16, &hE1, &h4D, &hC, &hFA, &hF1, &h11, &hF1, &h0, &h0, &h8 }, _
    { &h16, &hF1, &h40, &hC, &hBA, &h24, &h11, &h31, &h0, &h0, &h8 }, _
    { &h61, &hE1, &hA7, &h8B, &h72, &h50, &h8E, &h1A, &h0, &h0, &h2 }, _
    { &h18, &hE1, &h4D, &hC, &h32, &h51, &h13, &hE3, &h0, &h0, &h8 }, _
    { &h17, &h31, &hC0, &h8B, &h12, &h13, &h41, &h31, &h0, &h0, &h6 }, _
    { &h3, &h21, &h8F, &h8B, &hF5, &hF3, &h55, &h33, &h0, &h0, &h0 }, _
    { &h13, &hE1, &h4D, &hC, &hFA, &hF1, &h11, &hF1, &h0, &h0, &h8 }, _
    { &h11, &hF1, &h43, &hC, &h20, &h31, &h15, &hF8, &h0, &h0, &h8 }, _
    { &h11, &hE4, &h3, &h4C, &h82, &hF0, &h97, &hF2, &h0, &h0, &h8 }, _
    { &h5, &h14, &h40, &hB, &hD1, &h51, &h53, &h71, &h0, &h0, &h6 }, _
    { &hF1, &h21, &h1, &hB, &h77, &h81, &h17, &h18, &h0, &h0, &h2 }, _
    { &hF1, &hE1, &h18, &hB, &h32, &hF1, &h11, &h13, &h0, &h0, &h0 }, _
    { &h73, &h71, &h48, &hB, &hF1, &hF1, &h53, &h6, &h0, &h0, &h8 }, _
    { &h71, &h61, &h8D, &h4B, &h71, &h72, &h11, &h15, &h0, &h0, &h6 }, _
    { &hD7, &hD2, &h4F, &hB, &hF2, &hF1, &h61, &hB2, &h0, &h0, &h8 }, _
    { &h1, &h1, &h11, &hB, &hF0, &hF0, &hFF, &hF8, &h0, &h0, &hA }, _
    { &h31, &h61, &h8B, &hB, &h41, &h22, &h11, &h13, &h0, &h0, &h6 }, _
    { &h31, &h61, &h8B, &hB, &hFF, &h44, &h21, &h15, &h0, &h0, &hA }, _
    { &h31, &h61, &h8B, &hB, &h41, &h32, &h11, &h15, &h0, &h0, &h2 }, _
    { &h71, &h21, &h1C, &hB, &hFD, &hE7, &h13, &hD6, &h0, &h0, &hE }, _
    { &h71, &h21, &h1C, &hB, &h51, &h54, &h3, &h67, &h0, &h0, &hE }, _
    { &h71, &h21, &h1C, &hB, &h51, &h54, &h3, &h17, &h0, &h0, &hE }, _
    { &h71, &h21, &h1C, &hB, &h54, &h53, &h15, &h49, &h0, &h0, &hE }, _
    { &h71, &h61, &h56, &hB, &h51, &h54, &h3, &h17, &h0, &h0, &hE }, _
    { &h71, &h21, &h1C, &hB, &h51, &h54, &h3, &h17, &h0, &h0, &hE }, _
    { &h2, &h1, &h29, &h8B, &hF5, &hF2, &h75, &hF3, &h0, &h0, &h0 }, _
    { &h2, &h1, &h29, &h8B, &hF0, &hF4, &h75, &h33, &h0, &h0, &h0 }, _
    { &h1, &h11, &h49, &hB, &hF1, &hF1, &h53, &h74, &h0, &h0, &h6 }, _
    { &h1, &h11, &h89, &hB, &hF1, &hF1, &h53, &h74, &h0, &h0, &h6 }, _
    { &h2, &h11, &h89, &hB, &hF1, &hF1, &h53, &h74, &h0, &h0, &h6 }, _
    { &h2, &h11, &h80, &hB, &hF1, &hF1, &h53, &h74, &h0, &h0, &h6 }, _
    { &h1, &h8, &h40, &h4B, &hF1, &hF1, &h53, &h53, &h0, &h0, &h0 }, _
    { &h21, &h21, &h15, &h8B, &hD3, &hC3, &h2C, &h2C, &h0, &h0, &hA }, _
    { &h1, &h21, &h18, &h8B, &hD4, &hC4, &hF2, &h8A, &h0, &h0, &hA }, _
    { &h1, &h11, &h4E, &hB, &hF0, &hF4, &h7B, &hC8, &h0, &h0, &h4 }, _
    { &h1, &h11, &h44, &hB, &hF0, &hF3, &hAB, &hAB, &h0, &h0, &h4 }, _
    { &h53, &h11, &hE, &hB, &hF4, &hF1, &hC8, &hBB, &h0, &h0, &h4 }, _
    { &h53, &h11, &hB, &hB, &hF2, &hF2, &hC8, &hC5, &h0, &h0, &h4 }, _
    { &h21, &h21, &h15, &hB, &hB4, &h94, &h4C, &hAC, &h0, &h0, &hA }, _
    { &h21, &h21, &h15, &hB, &h94, &h64, &h1C, &hAC, &h0, &h0, &hA }, _
    { &h21, &hA1, &h16, &h8B, &h77, &h60, &h8F, &h2A, &h0, &h0, &h6 }, _
    { &h21, &hA1, &h19, &h8B, &h77, &h60, &hBF, &h2A, &h0, &h0, &h6 }, _
    { &hA1, &hE2, &h13, &h8B, &hD6, &h60, &hAF, &h2A, &h0, &h0, &h2 }, _
    { &hA2, &hE2, &h1D, &h8B, &h95, &h60, &h24, &h2A, &h0, &h0, &h2 }, _
    { &h32, &h61, &h9A, &h8B, &h51, &h60, &h19, &h39, &h0, &h0, &hC }, _
    { &hA4, &hE2, &h12, &h8B, &hF4, &h60, &h30, &h2A, &h0, &h0, &h2 }, _
    { &h21, &h21, &h16, &hB, &h63, &h63, &hE, &hE, &h0, &h0, &hC }, _
    { &h31, &h21, &h16, &hB, &h63, &h63, &hA, &hB, &h0, &h0, &hC }, _
    { &h21, &h21, &h1B, &hB, &h63, &h63, &hA, &hB, &h0, &h0, &hC }, _
    { &h20, &h21, &h1B, &hB, &h63, &h63, &hA, &hB, &h0, &h0, &hC }, _
    { &h32, &h61, &h1C, &h8B, &h82, &h60, &h18, &h7, &h0, &h0, &hC }, _
    { &h32, &hE1, &h18, &h8B, &h51, &h62, &h14, &h36, &h0, &h0, &hC }, _
    { &h31, &h22, &hC3, &hB, &h87, &h8B, &h17, &hE, &h0, &h0, &h2 }, _
    { &h71, &h22, &hC3, &hF, &h8E, &h8B, &h17, &hE, &h0, &h0, &h2 }, _
    { &h70, &h22, &h8D, &hB, &h6E, &h6B, &h17, &hE, &h0, &h0, &h2 }, _
    { &h24, &h31, &h4F, &hB, &hF2, &h52, &h6, &h6, &h0, &h0, &hE }, _
    { &h31, &h61, &h1B, &hB, &h64, &hD0, &h7, &h67, &h0, &h0, &hE }, _
    { &h31, &h61, &h1B, &hB, &h61, &hD2, &h6, &h36, &h0, &h0, &hC }, _
    { &h31, &h61, &h1F, &hB, &h31, &h50, &h6, &h36, &h0, &h0, &hC }, _
    { &h31, &h61, &h1F, &hB, &h41, &hA0, &h6, &h36, &h0, &h0, &hC }, _
    { &h21, &h21, &h9A, &h8B, &h53, &hA0, &h56, &h16, &h0, &h0, &hE }, _
    { &h21, &h21, &h9A, &h8B, &h53, &hA0, &h56, &h16, &h0, &h0, &hE }, _
    { &h61, &h21, &h19, &hB, &h53, &hA0, &h58, &h18, &h0, &h0, &hC }, _
    { &h61, &h21, &h19, &hB, &h73, &hA0, &h57, &h17, &h0, &h0, &hC }, _
    { &h21, &h21, &h1B, &hB, &h71, &hA1, &hA6, &h96, &h0, &h0, &hE }, _
    { &h85, &hA1, &h91, &hB, &hF5, &hF0, &h44, &h45, &h0, &h0, &h6 }, _
    { &h7, &h61, &h51, &hB, &hF5, &hF0, &h33, &h25, &h0, &h0, &h6 }, _
    { &h13, &h11, &h8C, &h80, &hFF, &hFF, &h21, &h3, &h0, &h0, &hE }, _
    { &h38, &hB1, &h8C, &h40, &hF3, &hF5, &hD, &h33, &h0, &h0, &hE }, _
    { &h87, &h22, &h91, &hB, &hF5, &hF0, &h55, &h54, &h0, &h0, &h6 }, _
    { &hB3, &h90, &h4A, &hB, &hB6, &hD1, &h32, &h31, &h0, &h0, &hE }, _
    { &h4, &hC2, &h0, &h0, &hFE, &hF6, &hF0, &hB5, &h0, &h0, &hE }, _
    { &h5, &h1, &h4E, &h80, &hDA, &hF0, &h15, &h13, &h0, &h0, &hA }, _
    { &h0, &h2, &h40, &h0, &h9, &hF7, &h53, &h94, &h0, &h0, &hE }, _
    { &hB0, &hD7, &hC4, &h8B, &hA4, &h40, &h2, &h42, &h0, &h0, &h0 }, _
    { &hE0, &h61, &hEC, &h0, &h6E, &h65, &h8F, &h2A, &h0, &h0, &hE }, _
    { &h30, &h35, &h35, &hB, &hF5, &hF0, &hF0, &h9B, &h0, &h0, &h2 }, _
    { &hF5, &hF6, &h9A, &h80, &hC, &h60, &hC7, &hA5, &h0, &h0, &hD }, _
    { &h53, &h0, &h85, &h0, &h3F, &h5F, &h6, &h7, &h1, &h0, &h6 }, _
    { &h20, &hE2, &h5B, &h80, &h0, &h50, &h16, &h15, &h0, &h0, &hA }, _
    { &h10, &h11, &h25, &h80, &hF0, &hD0, &h5, &h4, &h0, &h0, &hE }, _
    { &h1, &h1, &h0, &h0, &hD8, &hD8, &h96, &h96, &h0, &h0, &hA }, _
    { &h6, &h0, &h0, &hB, &hF4, &hF6, &hA0, &h46, &h0, &h0, &hE }, _
    { &h11, &h1, &h8A, &h4B, &hF1, &hF1, &h11, &hB3, &h0, &h0, &h6 }, _
    { &h0, &h0, &h40, &hB, &hD1, &hF2, &h53, &h56, &h0, &h0, &hE }, _
    { &h32, &h11, &h44, &h0, &hF8, &hF5, &hFF, &h7F, &h0, &h0, &hE }, _
    { &h0, &h2, &h40, &h0, &h9, &hF7, &h53, &h94, &h0, &h0, &hE }, _
    { &h1, &h1, &h80, &h80, &h5F, &h5F, &hB, &hB, &h1, &h1, &h4 }, _
    { &h0, &h13, &h50, &hB, &hF2, &hF2, &h70, &h72, &h0, &h0, &hE }, _
    { &h7, &h5, &h40, &hB, &h9, &hF6, &h53, &h94, &h0, &h0, &hE }, _
    { &h0, &h0, &hF, &h0, &h91, &h52, &h5, &h6, &h0, &h2, &h0 }, _
    { &h0, &h0, &hB, &hB, &hA8, &hD6, &h4C, &h4F, &h0, &h0, &h0 }, _
    { &h1C, &hC, &h1E, &hB, &hE5, &h5D, &h5B, &hFA, &h0, &h0, &hE }, _
    { &h31, &h16, &h87, &h80, &hA1, &h7D, &h11, &h45, &h0, &h0, &h8 }, _
    { &h30, &h10, &h90, &h0, &hF4, &hF4, &h49, &h33, &h0, &h0, &hC }, _
    { &h24, &h31, &h54, &h0, &h55, &h50, &hFD, &h2D, &h0, &h0, &hE } _
}

dim shared ibank_map(0 to 127) as integer => { _
 1,  0,  1,  4,  5,  2,  4,  3, 16, 17, 18, 20, 19, 20, 19, 21,  6,  6,  6,  7, _
 7,  7,  8,  8, 62, 63, 62, 63, 38, 39, 38, 39, 90, 22, 52, 98, 97, 99, 112, 54, _
112, 96, 68, 89, 80, 53, 112, 80, 49, 44, 49, 45, 40, 41, 42, 42, 43, 46, 46, 24, _
25, 27, 29, 104, 32, 32, 33, 33, 36, 37, 35, 35, 73, 73, 72, 72, 74, 75, 64, 65, _
66, 67, 71, 71, 68, 69, 70, 22, 56, 59, 57, 57, 60, 60, 58, 61, 61, 11, 11, 10, _
15, 9, 14, 13, 12, 107, 106, 77, 78, 78, 76, 121, 47, 117, 118, 114, 127, 116, 116, 119, _
107, 13, 55, 116, 123, 105, 94, 123 }

