


(ps-landscape-mode t)

(defun time-stamp-hh:mm ()
  "Return the current time as a string in \"HH:MM:SS\" form."
  (concat (substring (current-time-string) 11 16) " Uhr"))

(setq ps-right-header
  (list "/pagenumberstring load" 'time-stamp-dd-mon-yy 'time-stamp-hh:mm:ss))

;;(setq ps-header-lines 2)

;;(set ps-spool-duplex nil)

;; Achten auf
;;(setq ps-lpr-command
;;(setq ps-lpr-switches

(setq ps-paper-type 'ps-a4)


(setq ps-print-prologue "% ISOLatin1Encoding stolen from ps_init.ps in GhostScript 2.6.1.4:
% If the ISOLatin1Encoding vector isn't known, define it.
/ISOLatin1Encoding where { pop } {
% Define the ISO Latin-1 encoding vector.
% The first half is the same as the standard encoding,
% except for minus instead of hyphen at code 055.
/ISOLatin1Encoding
StandardEncoding 0 45 getinterval aload pop
    /minus
StandardEncoding 46 82 getinterval aload pop
%*** NOTE: the following are missing in the Adobe documentation,
%*** but appear in the displayed table:
%*** macron at 0225, dieresis at 0230, cedilla at 0233, space at 0240.
% \20x
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent
    /dieresis /.notdef /ring /cedilla /.notdef /hungarumlaut /ogonek /caron
% \24x
    /space /exclamdown /cent /sterling
	/currency /yen /brokenbar /section
    /dieresis /copyright /ordfeminine /guillemotleft
	/logicalnot /hyphen /registered /macron
    /degree /plusminus /twosuperior /threesuperior
	/acute /mu /paragraph /periodcentered
    /cedilla /onesuperior /ordmasculine /guillemotright
	/onequarter /onehalf /threequarters /questiondown
% \30x
    /Agrave /Aacute /Acircumflex /Atilde
	/Adieresis /Aring /AE /Ccedilla
    /Egrave /Eacute /Ecircumflex /Edieresis
	/Igrave /Iacute /Icircumflex /Idieresis
    /Eth /Ntilde /Ograve /Oacute
	/Ocircumflex /Otilde /Odieresis /multiply
    /Oslash /Ugrave /Uacute /Ucircumflex
	/Udieresis /Yacute /Thorn /germandbls
% \34x
    /agrave /aacute /acircumflex /atilde
	/adieresis /aring /ae /ccedilla
    /egrave /eacute /ecircumflex /edieresis
	/igrave /iacute /icircumflex /idieresis
    /eth /ntilde /ograve /oacute
	/ocircumflex /otilde /odieresis /divide
    /oslash /ugrave /uacute /ucircumflex
	/udieresis /yacute /thorn /ydieresis
256 packedarray def
} ifelse

/reencodeFontISO { %def
  dup
  length 5 add dict			% Make a new font (a new dict
					% the same size as the old
					% one) with room for our new
					% symbols.

  begin					% Make the new font the
					% current dictionary.


    { 1 index /FID ne
      { def } { pop pop } ifelse
    } forall				% Copy each of the symbols
					% from the old dictionary to
					% the new except for the font
					% ID.

    /Encoding ISOLatin1Encoding def	% Override the encoding with
					% the ISOLatin1 encoding.

    % Use the font's bounding box to determine the ascent, descent,
    % and overall height; don't forget that these values have to be
    % transformed using the font's matrix.
    FontBBox
    FontMatrix transform /Ascent exch def pop
    FontMatrix transform /Descent exch def pop
    /FontHeight Ascent Descent sub def

    % Define these in case they're not in the FontInfo (also, here
    % they're easier to get to.
    /UnderlinePosition 1 def
    /UnderlineThickness 1 def

    % Get the underline position and thickness if they're defined.
    currentdict /FontInfo known {
      FontInfo

      dup /UnderlinePosition known {
	dup /UnderlinePosition get
	0 exch FontMatrix transform exch pop
	/UnderlinePosition exch def
      } if

      dup /UnderlineThickness known {
	/UnderlineThickness get
	0 exch FontMatrix transform exch pop
	/UnderlineThickness exch def
      } if

    } if

    currentdict				% Leave the new font on the
					% stack

    end					% Stop using the font as the
					% current dictionary.

    definefont				% Put the font into the font
					% dictionary

    pop					% Discard the returned font.
} bind def

/Font {
  findfont exch scalefont reencodeFontISO
} def

/F {					% Font select
  findfont
  dup /Ascent get /Ascent exch def
  dup /Descent get /Descent exch def
  dup /FontHeight get /FontHeight exch def
  dup /UnderlinePosition get /UnderlinePosition exch def
  dup /UnderlineThickness get /UnderlineThickness exch def
  setfont
} def

/FG /setrgbcolor load def

/bg false def
/BG {
  dup /bg exch def
  { mark 4 1 roll ] /bgcolor exch def } if
} def

/dobackground {				% width --
  currentpoint
  gsave
    newpath
    moveto
    0 Ascent rmoveto
    dup 0 rlineto
    0 Descent Ascent sub rlineto
    neg 0 rlineto
    closepath
    bgcolor aload pop setrgbcolor
    fill
  grestore
} def

/dobackgroundstring {			% string --
  stringwidth pop
  dobackground
} def

/dounderline {				% fromx fromy --
  currentpoint
  gsave
    UnderlineThickness setlinewidth
    4 2 roll
    UnderlinePosition add moveto
    UnderlinePosition add lineto
    stroke
  grestore
} def

/eolbg {
  currentpoint pop
  PrintWidth LeftMargin add exch sub dobackground
} def

/eolul {
  currentpoint exch pop
  PrintWidth LeftMargin add exch dounderline
} def

/SL {					% Soft Linefeed
  bg { eolbg } if
  ul { eolul } if
  currentpoint LineHeight sub LeftMargin exch moveto pop
} def

/HL /SL load def			% Hard Linefeed

/sp1 { currentpoint 3 -1 roll } def

% Some debug
/dcp { currentpoint exch 40 string cvs print (, ) print = } def
/dp { print 2 copy
   exch 40 string cvs print (, ) print = } def

/S {
  bg { dup dobackgroundstring } if
  ul { sp1 } if
  show
  ul { dounderline } if
} def

/W {
  ul { sp1 } if
  ( ) stringwidth			% Get the width of a space
  pop					% Discard the Y component
  mul					% Multiply the width of a
					% space by the number of
					% spaces to plot
  bg { dup dobackground } if
  0 rmoveto
  ul { dounderline } if
} def

/BeginDSCPage {
  /vmstate save def
} def

/BeginPage {
  PrintHeader {
    PrintHeaderFrame { HeaderFrame } if
    HeaderText
  } if
  LeftMargin
  BottomMargin PrintHeight add
  moveto				% move to where printing will
					% start.
} def

/EndPage {
  bg { eolbg } if
  ul { eolul } if
  showpage				% Spit out a page
} def

/EndDSCPage {
  vmstate restore
} def

/ul false def

/UL { /ul exch def } def

/h0 14 /Helvetica-Bold Font
/h1 12 /Helvetica Font

/h1 F

/HeaderLineHeight FontHeight def
/HeaderDescent Descent def
/HeaderPad 2 def

/SetHeaderLines {
  /HeaderOffset TopMargin 2 div def
  /HeaderLines exch def
  /HeaderHeight HeaderLines HeaderLineHeight mul HeaderPad 2 mul add def
  /PrintHeight PrintHeight HeaderHeight sub def
} def

/HeaderFrameStart {
  LeftMargin BottomMargin PrintHeight add HeaderOffset add
} def

/HeaderFramePath {
  PrintWidth 0 rlineto
  0 HeaderHeight rlineto
  PrintWidth neg 0 rlineto
  0 HeaderHeight neg rlineto
} def

/HeaderFrame {
  gsave
    0.4 setlinewidth
    HeaderFrameStart moveto
    1 -1 rmoveto
    HeaderFramePath
    0 setgray fill
    HeaderFrameStart moveto
    HeaderFramePath
    gsave 1 setgray fill grestore      % AE + SMW
    gsave 0 setgray stroke grestore
  grestore
} def

/HeaderStart {
  HeaderFrameStart
  exch HeaderPad add exch
  HeaderLineHeight HeaderLines 1 sub mul add HeaderDescent sub HeaderPad add
} def

/strcat {
  dup length 3 -1 roll dup length dup 4 -1 roll add string dup
  0 5 -1 roll putinterval
  dup 4 2 roll exch putinterval
} def

/pagenumberstring {
  PageNumber 32 string cvs
  ShowNofN {
    (/) strcat
    PageCount 32 string cvs strcat
  } if
} def

/HeaderText {
  HeaderStart moveto

  HeaderLinesRight HeaderLinesLeft
  Duplex PageNumber 1 and 0 eq and { exch } if

  {
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall

  HeaderStart moveto

   {
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      dup stringwidth pop
      PrintWidth exch sub HeaderPad 2 mul sub 0 rmoveto
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall
} def

/ReportFontInfo {
  2 copy
  /t0 3 1 roll Font
  /t0 F
  /lh FontHeight def
  /sw ( ) stringwidth pop def
  /aw (01234567890abcdefghijklmnopqrstuvwxyz) dup length exch
  stringwidth pop exch div def
  /t1 12 /Helvetica-Oblique Font
  /t1 F
  72 72 moveto
  gsave
    (For ) show
    128 string cvs show
    ( ) show
    32 string cvs show
    ( point, the line height is ) show
    lh 32 string cvs show
    (, the space width is ) show
    sw 32 string cvs show
    (,) show
  grestore
  0 FontHeight neg rmoveto
  (and a crude estimate of average character width is ) show
  aw 32 string cvs show
  (.) show
  showpage
} def

% 10 /Courier ReportFontInfo
")
