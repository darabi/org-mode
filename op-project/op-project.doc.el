;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;        Fast vollständige Dokumentation Durcheinander-Dokumentation für den
;;                          PROJECT-MODE des EMACS,
;;	  geschrieben von November 1994 bis November 1996 von 
;;	  
;;	  Dr. Stefan Walther
;;	  Rudolf-Diesel-Str. 3
;;	  55131 Mainz
;;	  Telefon: xx49/6131/53542
;;	  
;;	  zur Zeit beschäftigt am 
;;	  Universitätsklinikum Mainz
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





Indentation

TAB			Special clever indentation (looks for many patterns, even of previous lines). 
			Two times in the same line usually toggles between two points.

M-;                     forced indentation, not respecting user wishes

C-c TAB                 indent-like-last-headline, setzt eine Ueberschriftszeile mit der Sternanzahl der letzten Ueberschrift ein

C-return                fixup-whitespace, macht aus mehreren Leerzeichen eines oder keines

op-indent-region	Indents region like going with the TAB around

ESC-TAB			inserts tab, if indentation is not ok
trim-buffer		removes spaces at the end of a line
reset-display		resets display and widens it

RETURN			invokes a pretty complicatet program, supports arrows (==>) at the begin of each comment

			Kompliziertes Indent für Outline- oder Project-Mode des Emacs.
			-- unter einer Outline-Überschrift: Cursor unter erste Schrift nach Sternen
			-- unter Freizeichen: an das Ende der Freizeichen (ignoriert dazwischen-
			   liegende Newlines)
			-- bei nur Newlines: kein Indent
			-- Buchstaben am Zeilenanfang: normale Indentierung
			-- Zeichen am Zeilenanfang: Indentierung nach dem ersten Leerraum
			-- Pfeil (==>) in der da überliegenden Zeile: Einsetzen von vier Blanks
			
			-- Toggeln der Cursor-Positionen bei mehrmaligem Tippen zwischen
			   Indentierung und Zeilenende
			-- automatisches Tabulatorisieren der Leerzeichen (tabify)

C-c C-g			Blendet, wenn nicht in Column-mode, die
			aktuellen Überschriften der momentanen Zeile
			ein. Mit Argument belndet es den Wert der
			selbstdefinierbaren Variablen colum-ruler ein,
			per Default eine Leiste von 1 bin 222

			
*** if M-c M-o is not anymore available as a key, please type M-x outline-minor-mode ***

*** Einige Funktionen brauchen zwei Newlines am Ende um sich korrekt zu verhalten !!! ***




C-c C-h		        Einsetzen einer Kurzsignatur (Variable "signature", setzbar im File op-ini.el, Datum, Uhrzeit)

\C-c\C-h"    insert-signature
\C-c\C-j"    insert-date-in-brackets
\C-c\C-k"    insert-date

\C-c\C-t     "Tel.: "
\C-c\C-f     "Fax:  "
\C-c\C-a     "==> Anruf am DATUM bei "
\C-c\C-a     "==> Erledigt am DATUM_im_Langformat











ÄÖÜäöüß durch Tippen von "A,"O,"U,"s  u.s.w.. 
  Richtiges "A kann man Tippen durch z.B.  " SPC A  oder  C-q " A




Rehighlights according to the value of hilit-auto-rehighlight, or the
prefix argument if that is specified.

	S-C-l		(control-shift-lowercase-L) repaint according to hilit-auto-rehighlight
	^U S-C-l	repaint entire buffer
	^U - S-C-l	repaint visible portion of buffer
	^U n S-C-l	repaint n lines to either side of point




`outline-minor-mode' Minor Mode Bindings Starting With C-c:
key		binding
---		-------

C-c C-o		Prefix Command

C-c C-o C-o	hide-other
C-c C-o C-q	hide-sublevels
C-c C-o C-k	show-branches
C-c C-o C-l	hide-leaves
C-c C-o C-e	show-entry
C-c C-o C-c	hide-entry
C-c C-o C-a	show-all
C-c C-o C-t	hide-body
C-c C-o C-b	outline-backward-same-level
C-c C-o C-f	outline-forward-same-level
C-c C-o C-u	outline-up-heading
C-c C-o C-d	hide-subtree
C-c C-o C-s	show-subtree
C-c C-o TAB	show-children
C-c C-o C-p	outline-previous-visible-heading
C-c C-o C-n	outline-next-visible-heading



`project-mode' Minor Mode Bindings Starting With C-c:
(Working like outline-minor-mode, but keeps hideif-definitions hidden)

key		binding
---		-------

M-c M-o		Prefix Command

M-c M-o M-o	hide-other-hide (escpecially useful after search)
M-c M-o M-q	hide-sublevels-hide
M-c M-o M-k	show-branches-hide
M-c M-o M-j	show-all-branches-hide

M-c M-o M-l	hide-leaves-hide
M-c M-o M-e	show-entry-hide
M-c M-o M-c	hide-entry-hide
M-c M-o M-a	show-all-hide
M-c M-o M-t	hide-body-hide
M-c M-o M-d	hide-subtree-hide
M-c M-o M-s	show-subtree-hide
M-c M-o TAB	show-children-hide
M-c M-o ^	top-heading
M-c M-o M-^	very-top-heading
M-c M-o v	next-top-heading

M-c M-o M-f	outline-forward-jump-same-level
M-c M-o M-b	outline-backward-jump-same-level

Um mit includeten Files zu arbeiten (" #include "path/filename" "):

M-c M-o M-i	load-files-at-include-statements
M-c M-o M-z	extract-and-save-includes

Diese beiden Statements laden alle Files, die ein label
     #include "Pfad/Filename"
im File haben, in den Text. Beim Abspeichern werden diese durch das
Kommando "write-file" wieder herausgeschrieben an die orginalen
Plätze. Änderungen u.a. werden somit weitergegeben. Bei gecrypteten
Files (Extention .crypt) kann bei einem diff hinterher eine
Veränderung angezeigt werden, dies sollte aber kein Problem
darstellen. Achtung: Um das Ende des Files zu markieren, wird nach
einem include der File und ein \r#end_include_statement eingefügt. 
Dieses bitte auf keinen Fall löschen --- sonst sind ernste Probleme beim
Abspeichern zu erwarten.

M-c M-o i	insert-file-at-include-statement (macht ein echtes
		Include mit Endmarke, kann nur mit Undo rückgängig gemacht werden)






ohne Key	show-really-everything,  macht widen und zeigt alle Steuerzeichen
sre		wie oben, aber zieht den Text wieder zusammen


M-c M-o >	op-shift-right -- ganze Region eine Sternstufe tiefer legen (mehr Sterne)
M-c M-o <	op-shift-left  -- ganze Region eine Sternstufe höher legen (weniger Sterne)

		! *Ausblenden* *ist* *jeweils* *eine* *Regions-Funktion* !
C-c C-o 1	ausblend-1  *E (erledigtes) und *FW (fällt weg) mit zugehörigem Text
C-c C-o 2	ausblend-2  *V (verschobenes) mit zugehörigem Text
C-c C-o 3	ausblend-3  *D (dringendes) mit zugehörigem Text

C-c C-o 5	ausblend-5  alles
C-c C-o 6	ausblend-6  #T (Terminzeilen)

		Doppeltes Ausblenden kann spezielles einblenden ermöglichen,
		z.B. 2 x ausblend-6, wenn nur Hauptüberschriften sichtbar sind.




remove-ellipses		  removes all ellipses (...) from the actual region into a copied buffer
make-report		  removes all ellipses, /*-comments and
			  (find-file...), (insert-file...) etc. from from the actual *region* into 
			  a copied buffer
remove-simple-comments	  simple function to remove comments (search for beginning 
			  and end and remove all between)	   
remove-simple-re-comments same and simple as above, but takes start and end as 
			  regular expressions (C-comments: "/\*" "\*/")
remove-/*-comments	  very very tricky and complicated function to remove all
			  _*valid*_  C-comments. Needs perl to be installed.


C-q C-m		aktuelle (Rest-)Zeile ausblenden, sofortiges C-d macht sie wieder sichtbar








M-c M-o M-x	xfig-text-extractor   ==> new window

in new window (buffer name "*xfig-text-in-figure*"):

Occur mode:
Major mode for output from xfig-text-extractor
Move point to one of the occurrences in this buffer,
then use C-c C-c to go to the same occurrence
in the buffer that the occurrences were found in.
Or click mouse-2 on an occurrence line.
key		binding
---		-------

C-c C-c		op-line-search (in project file if there)
C-c C-s		op-sort-lines

C-c		Prefix Command (for xfig-figures)
mouse-2		occur-mode-mouse-goto (in xfig figure file +- 1 line, 
					only before sorting)


Change from text to new window with C-x o




M-c M-o M-y	corell-text-extractor	==> new window

in new window (buffer name "*corell-text-in-figure*"):

key		binding
---		-------

C-c C-c		op-line-search (in project file if there)
C-c C-s		op-sort-lines

Change from text to new window with C-x o







M-c M-o M-m	mpx-text-extractor   ==> new window

in new window (buffer name "*mpx-text*"):

key		binding
---		-------

C-c C-c		op-line-search (in project file if there)
C-c C-s		op-sort-lines

Change from text to new window with C-x o










extra Searching:

M-s		search-forward-for-current-word, with argument: search only for marks like .word.
M-r		search-backward-for-current-word
M-n		search-forward-visible-expand : searches for the next visible occurence of 
		       the actual word and expands tree locally (try twice after change of 
		       direction)
M-p		search-backward-visible-expand

agrep		agrep command, occur in a new window
		new parameters gan be given at the beginning of the searchstring

in the new window:

C-c C-c		find-agrep-occur, jump to line in other buffer
C-x o		back to agrep-occur-window





Marking:

C-x h		mark-buffer-as-region
C-SPC		set mark here (cursor position gives the other end of the region)




Utilities

M-C		capitalize-current-word (Uppercase C)
M-u		upcase-current-word, goto next one
M-l		downcase-current-word, goto next one

replace-umlaute	    replaces ü with ue and so on



Hyperbole-Mode (nicht zu verwechselnmit dem seltsamen Teil von Motorola !)

Auslösen durch Alt-Return oder Shift-Mausknopf-2. 
Buttons müssen das Zeichen -> vornedran haben. Alles weitere in den
folgenden Beispielen (aus dem File op-hyper.el)

;;
;; Kettenausführungen, genaueres siehe unter den Beispielen von EVAL
;   ->FILE:op-hyper.el%defun;EVAL:date
;   ->FILE:op-hyper.el%defun->;EVAL:date
;
;;;;;;;;;
;;
;; Beispiele SQL:
;;  
;   ->SQL:sqlfile%startpunkt
;   ->SQL:sqlfile2
;
wobei hier der File "Sqlfile"im aktuellen Directory notwendig ist:
start sqlfile
   startpunkt
     (message "Nur ein Test für SQL per Lisp")
   END
   asdfa
   sfdasf
   dasdfasdf
end sqlfile

Hierbei wird, wenn so angegeben, der Rest des Files (im Beispiel: von startpunkt bis END) ausgeblendet,
so daß da beliebiges drumherum stehen darf.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kettenausführungen, genaueres siehe unter den Beispielen von EVAL
;   ->FILE:op-hyper.el%defun;EVAL:date
;   ->FILE:op-hyper.el%defun->;EVAL:date
;
;
;;;;;;;;;;;;;;;; Beispielliste aus den Kommentaren (z.T. erweitert) ;;;
;;
;; Beispiele SQL:
;;  
;   ->SQL:sqlfile.sql%startpunkt
;   ->SQL:sqlfile2.sql
;
;;
;; Beispiele FILE:
;; 
;;   ->>Wort sucht alle vorkommenden ganzen Wörter, ->Wort dieses nur in Überschriften, 
;;   ->*Wort dieses nur am Anfang einer Einser-Überschrift, entsprechend ->**Wort bei einer Zweier-Überschrift
;; 
;   ->>FILE:../op-project.el%defun     ; mehrmals auslösen gibt jeweils das nächste Muster
;   ->>FILE:../op-project.el%defun->
;   ->>FILE:defun
;   ->>FILE:defun->
;   ->>FILE:../op-project.el%          ; Sucht nur den anderen File aus
;   ->>defun                           ; Defaultbutton: sucht im gleichen File das Wort "defun"
;   ->>defun->                         ; dito, setzt den Cursor aber in ein anderes Fenster
;   ->>../op-project.el%               ; sucht den File ../op-project.el heraus
;   ->../op-project.el%                ; sucht den File ../op-project heraus (-> geht hier nur, weil kein Einspring-
;                                      ;           punkt vorhanden ist, wigentlich muesste hier ->> stehen
;   ->>../op-project.el%defun->        ; in anderem File
;   ->>FILE:[ab]test                   ; alle Buttons dürfen Regexe enthalten, jedoch keine Blanks
;        atest btest ctest             ;    (bleibt bei atest und btest stehen, kennt aber kein ctest)
;   ->>FILE:"a test"->                 ; Blanks usw. sind erlaubt, doch nur in Anführungsstrichen  ("a test")
;   ->>FILE:"\* test"                  ; Achtgeben: Sterne gehören zu den Regexen, müssen daher gequotet werden
;   ->>FILE:"\* test"->
;   * test    $ test                   ; nur zum Testen
;
;   ->>A:Müller                        ; private Kurzform für ->FILE:adressen.op%"name", Adressenfile als Variable hier im Code
;
;   ->**Ueberschriftentest             ; steht ein * am Anfang des Musters, werden die Sterne nicht als Regexe interpretiert,
;                                      ;    sondern als "Uberschriftengerechtes Muster" (hier: `**	Ueberschriftentest')
;                                      ;    Verhalten: ist das Muster am Zeilenanfang zu finden, wird es auch dort genommen,
;                                      ;    ansonsten geht es durch den normalen Text.
; 
;   ->Ueberschriftentest
;   ->FILE:Ueberschriftentest
;
**	Ueberschriftentest
;
;;
;; Beispiele FUNC:
;;  
;   ->FUNC:funcfile%startpunkt
;   ->FUNC:funcfile%Fakultät
;   ->FUNC:startpunkt
;
wobei hier der File funcfile notwendig ist:
start funcfile
  asdf asdf asdf asdf asd f
  startpunkt
     (message "%d" (* 5 4 3 2 1))
  END

  Fakultät
     (message "Fakultät von 5: %d" (* 5 4 3 2 1))
  END
  as asfd asdf asdf 
end funcfile

Hierbei wird, wenn so angegeben, der Rest des Files (im Beispiel: von startpunkt bis END und von 
Fakultät bis END) ausgeblendet, so daß da beliebiges drumherum stehen darf.

;;
;; Beispiele EVAL:
;;  
;   ->EVAL:xeyes
;   ->EVAL:xeyes&
;   ->EVAL:"date --version"  
;   ->EVAL:"gcc -v"
;   ->EVAL:xfig%test.fig
;   ->EVAL:"xfig -bg red"%test.fig    ; Argumente in Anführungsstrichen, Ausführungspunkt NUR vor dem 
;                                     ; ersten Blank (beim "->EVAL:xxxx"
;   ->EVAL:"xfig -bg red"%test.fig&   ; wie eben
;   ->FILE:op-hyper.el%defun;EVAL:"date --version"  
;   ->FILE:op-hyper.el%defun->;EVAL:"date --version"  
;				      ; bei Kettenargumenten: EVAL immer an den Schluß,
;                                     ;  andere Funktionen immer mit %-Zeichen. Nur je ein Buttontyp ist erlaubt.
;   ->EVAL:test.fig                   ; Defaultextention ist einprogrammiert (siehe oben)
;   ->!date                           ; Defaultbuttons
;   ->!"date --version"
;   ->!cat%op-hyper.el
;  
;;
;; Beispiele REF:
;;
;   ->REF:../op-hyper/op-hyper.el%1button      ; Muster ist hier "1button", kann aber beliebig sein
;   ->REF:../op-hyper/op-hyper.el%1button->
;   ->REF:1button                              ; auch hier: mehrmaliges Auslösen ohne Bewegen des 
;                                              ; Cursors im anderen Fenster sucht nächste Referenz
;   ->REF:1button->
;
; die alle sollten hier enden:   ->1button:FILE:defun   ; Doppelpunkt ist günstig, aber nicht Pflicht:
;                                ->1button#FILE:defun   ; geht z. B. auch
;
;
;;
;; Beispiele (siehe auch die bei REF:func) REF-R:
;;
;   ->REF-R:../op-hyper/op-hyper.el%2button->      ; Muster ist hier "2button", kann aber beliebig sein
;
;   ->REF-R:2button->
;
;   ->2button:REF-R:3button->
;
; die alle sollten hier enden:   ->3button:FILE:das_hier_gibts_nicht   
;
;; weiteres Beispiel: Loop zurück zum ersten Button wird nicht unendlich durchlaufen 
;; (ideal zum Einlesen vieler Files !!!)
;
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:4button->
;
;
;; nicht gefunden wird dagegen die folgende Loop (gibt dann aber einen Emacs-Fehler)
;; (zum Debuggen empfiehlt sich REF-R:func in REF:func umzudefinieren) 
;   ->4button:REF-R:5button->           
;        ->5button:REF-R:6button->
;             ->6button:REF-R:5button->
;
;



An den Hyperbole-Mode angeschlossen (und auch u.a. auf ihn abgestimmt)
ist der automatic-change-mode:
Ein- und austoggeln durch den Befehl automatic-change-mode, ein mit
Argument (aus geht nicht durch Argument).

Dieser Mode ändert bei den Labels -> und <- automatisch alle gleich
aussehenden Textstücke mit (damit eine Datenkonsistenz erhalten
bleibt). Warnanzeige ist mit dabei. Bei Labels des Typs
->erster_Teil%zweiter_Teil wird NUR der erste Teil synchron
mitgeändert, der Zweite teil bleibt erhalten.
Kann bei einer entsprechenden Anzahl von Labels beim Ändern der Labels
etwas Zeit kosten.
;;  zum Test: hier jeweils etwas dem Wort "[]test", "+test", "<>test" und an dem Wort "->test" ändern !
;;  (Bei normalem Text: Achtgeben mit Kommatas !)
;;
;;   []test
;;                              <>test
;;	[]test 	      #
;;                              <>test  
;;	[]test		#
;;                              <>test
;;    []tt     test
;;                              <>test
;;	   []test
;;
;; ->test   ->test
;;  +test   +test
;;
;;  zum anderen Test: Am Wort "link-test" sollte sich immer etwas aendern, nach dem Prozentzeichen
;;     jedoch nur noch an denen mit einem gleichen Ende.
;;     (siehe Dokumentation zu op-hyper.el)
;;
;;	 ->link-test%mit_einem_ende
;;	 ->link-test%mit_einem_ende
;;
;;	 ->link-test%mit_einem_anderen_ende
;;









Column-mode

Zwischen den Seuterzeichen %%%C string und %%%C gleicher String (z.B.
%%%C 1  
   Testtext
   %%%C 1  )
gelten leicht veränderete Editierregeln (Die Marken müssen nicht am
Zeilenanfang stehen !). Die Tasten

(local-set-key [C-down] 'quarter-down)
(local-set-key [C-up] 'quarter-up)
(local-set-key [C-right] 'quarter-right)
(local-set-key [C-left] 'quarter-left)
(local-set-key [C-tab] 'quarter-tab-right)

sind neu belegt worden, wobei alle Funktionen mit "quarter"im Namen
das Viertel zwischen 3 Uhr und 6 Uhr vom Cursor meinen. 
Die alten Funktionen sind umbelegt worden auf

(local-set-key [?\C->] '(lambda () (interactive) (scroll-right 2) (backward-char 2)))
(local-set-key [?\C-<]  '(lambda () (interactive) (scroll-left 2) (forward-char 2)))
(local-set-key [M-down]  '(lambda () (interactive) (save-excursion (scroll-next-line))))
(local-set-key [M-up]	'(lambda () (interactive) (save-excursion (scroll-previous-line))))

Die beiden folgenden Funktionen verhalten sich anders: Sie suchen
nicht mehr das Wort auf dem sie stehen in gleichen Fenster, sondern
machen dafür ein neues Fenster auf (bzw. nutzen ein bereits
vorhandenes). Damit lassen sich zu bestimmten Kürzeln sehr gut die
dazugehörigen Textstellen mit dem gleichen String heraussuchen. Bitte
achtgeben: Das gesuchte Wort darf nicht mit einer runden Klammer beginnen !

(local-set-key "\M-s" 'search-this-string-forward), mit Argument: suche nach !Wort! -Marken
(local-set-key "\M-r" 'search-this-string-backward)


Außerdem gibt es die Funktion sum-column, die eine Spalte von der
Marke bis zum Punkt addiert.



Arbeitet eng zusammen mit dem picture-mode:

M-x p	 picture-mode
C-c C-c  Verlassen des Picture-modes


Picture:right mode:
Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \   Move southeast (se) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement. 
 
You can move around in the buffer with these commands:
  C-n	  Move vertically to SAME column in previous line.
  C-p	  Move vertically to SAME column in next line.
  C-e	  Move to column following last non-whitespace character.
  C-f	  Move right inserting spaces if required.
  C-b	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.

You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines "interesting character".  You can manually
	    change the tab stop list with command M-x edit-tab-stops.

You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  DEL  Clear (replace) ARG columns before point, moving back over them.
  C-k	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  C-o	  Open blank line(s) beneath current line.

You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  C-x r r   Copies a rectangle to a register.
  C-x u   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  picture-mode-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys.

















Hide-Ifdef minor mode (indicator Ifdef):
Toggle Hide-Ifdef mode.	 This is a minor mode, albeit a large one.
With ARG, turn Hide-Ifdef mode on iff arg is positive.
In Hide-Ifdef mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.	 Initially, the global value of `hide-ifdef-env'
	is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.
	Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
	and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
	from one of the lists in `hide-ifdef-define-alist'.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.



`hide-ifdef-mode' Minor Mode Bindings Starting With C-c:
key		binding
---		-------

C-c C-q		hide-ifdef-toggle-read-only
C-c C-s		show-ifdef-block
C-c C-d		hide-ifdef-block
C-c ESC		Prefix Command

C-c ESC s	show-ifdefs
C-c ESC h	hide-ifdefs
C-c ESC U	hide-ifdef-use-define-alist
C-c ESC D	hide-ifdef-set-define-alist
C-c ESC u	hide-ifdef-undef
C-c ESC d	hide-ifdef-define

hide-ifdef-mode bindings in op-hide.el:

M-c M-s		show-ifdefs-in-region
M-c M-h		hide-ifdefs-in-region




Logbuch-Mode ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Verhält sich in etwa so:

C-c C-c im Project-Mode-Buffer sendet diesen Buffer an einen File, der
in der Variablen  current-logbuch-file  definiert ist.
C-c C-r tut dieses mit einer Region aus diesem Buffer.

Tippt man den Befehl logbuch ein (oder hat sich nach einer Zeit
logbuch-alarm-on dieser Befehl ausgelöst), dann gibt es zwei
Möglichkeiten:
Ist die Variable use-logbuch-projectdoc-buffer auf t gesetzt, schaltet
dieser Befehl auf den Buffer logbuch-projectdoc-buffer. Dieser wird
dann durch C-c C-c oder C-c C-r gespeichert und in den Hintergrund
geschoben. Ist diese Variable nicht gesetzt, dann wird ein Buffer
"*logbuch*" erzeugt, der dann durch C-c C-c ganz oder durch C-c C-r
mit der gesicherten Region an den File current-logbuch-file angehängt
wird. Das Anhängen geschieht jeweils mit einem Vor- und einem
Nachspann, der in den Variablen 
(setq logbuch-trenner-anfang  "<++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>")
(setq logbuch-trenner-ende "")
(setq signature "SMW")
gespeichert ist.

Ist der logbuch-projectdoc-buffer ein gecrypteter File, dann sollte
man einmal am Anfang per Hand den Befehl "logbuch" aufrufen, damit das
Passwort abgefragt werden kann.
Falls mal was schiefgeht, sollte das Aufrufen von "logbuch" manches wieder beheben.



key		binding
---		-------

C-c C-r		logbuch-send-region
C-c C-c		logbuch-send-and-exit
C-c C-s		logbuch-set-document-buffer

Global Bindings Starting With C-c:
key		binding
---		-------



Für den .ini-File:

(defvar use-logbuch-projectdoc-buffer t)   ;; Zu einen neuen Buffer (=nil) oder zum Buffer  logbuch-projectdoc-buffer  springen (=t) ?
(defvar logbuch-projectdoc-buffer "~/projects/myprojects.op.crypt")
(defvar current-logbuch-file "~/projects/Wochenbericht.txt")  ;; an diesen File sollen die Logbuch-Kommentare angehängt werden

(logbuch-alarm-on 90) ;; alle 90 Minuten Nachfragen

 

Funktionen (nicht alle gut getestet, aber die meisten):

logbuch			      (not bound to any keys)
  Function: Edit a message to be sent.	Argument means resume editing (don't erase).
logbuch-alarm-off		      (not bound to any keys)
  Function: Turn the Emacs LOGBUCH alarm off.
logbuch-alarm-on		      (not bound to any keys)
  Function: Turn the Emacs LOGBUCH alarm on.  The alarm goes off every INTERVAL minutes
  To change the value of INTERVAL, switch logbuch-alarm off and on again.
logbuch-mode		      (not bound to any keys)
  Function: Major mode for editing text to be sent to logbuch.
logbuch-other-window	      (not bound to any keys)
  Function: Like `logbuch' command, but display logbuch buffer in another window.
logbuch-send-and-exit	      (not bound to any keys)
  Function: Send message like logbuch-send-buffer, then, if no errors, exit from logbuch buffer.
logbuch-send-buffer	      (not bound to any keys)
  Function: Send the message in the current buffer to logbuch.
logbuch-send-region	      (not bound to any keys)
  Function: Send the message in the current region to logbuch.


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






EnCrypting and DeCrypting (crypt-encryped-mode)

--- It was REALLY complicated to get crypt an hilight to work together.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*** WARNINGS *** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*** PROBLEMS *** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*** BUGS *** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


The next two warnings should not be valid in the project mode

--- If you want to use pure crypt functions --- please use all functions ***WITHOUT*** highlighting !!

--- Do not use RCS (or SCCS) and pure crypt functions at the same time within the emacs


These are valid also in project mode

--- Delete the auto-save-files afterwards (#filename#) (with rm -i \#filename\#)

--- if C-x C-sasks you for encrypting a buffer that should NOT be encrypted:
    -- ! kill the buffer with the extention .crypt !
    -- ! save the original buffer with M-x save-buffer !
    -- ! if you want to correct this: ESC ESC (setq use-keymap-for-crypted-files nil) !
        OR do C-x C-e at the end of the next line: (setq use-keymap-for-crypted-files nil)



Encrypted files should have the extention .op.crypt.
In encrypted mode exists a duplicated buffer with extention .op.crypt ==>
    don't change something in THIS buffer, just leave it [or kill it (C-x k)]


C-x C-w	       ask for crypting and write file

C-x C-s	       ask the first time for crypting and write file
	       (Bug report: If you are saving on times crypted and
	       another time times not, there might be occour a problem. Kill the
	       buffer with the extention .op.crypt in that case and try it again)

ask-for-crypt-save
  Function: Asks for encrypted save of the file -- not planned to use
  interactively (better use C-x C-w)
ask-for-crypt-write
  Function: Asks for encrypted write of the file -- not planned to use
  interactively (better use C-x C-w)
 
save-buffer-crypted		     
  Function: Saves the file in a crypted format (like C-x C-s)
write-file-crypted
  Function: Writes the file in a crypted format (like C-x C-w)



;; !!!!!!!!!!!!
;;
;; If you want to use crypt --- please use all functions ***WITHOUT*** highlighting !!
;;
;;
;; crypt-bind-insert-file
;;   Function: Bind `crypt-insert-file' in place of `insert-file' or reverse based on
;;   Variable: *t value means bind `crypt-insert-file' over `insert-file'.
;; crypt-encode-region
;;   Function: Encode region START to END.	 When called interactively START and END
;; crypt-encoded-mode
;;   Function: Toggle encoded mode.  With ARG, turn on iff positive, otherwise turn off.
;; crypt-encrypt-region
;;   Function: Encrypt region START to END using KEY and `crypt-encryption-type'.	When
;; crypt-encrypted-mode
;;   Function: Toggle encrypted mode.  With ARG, turn on iff positive, otherwise turn off.
;; crypt-insert-file	      C-x i
;;   Function: Insert decoded/decrypted contents of file FILENAME into buffer after point.
;; crypt-rebuild-tables
;;   Function: Rebuilds the encryption and encoding tables and `minor-mode-alist'.
;; crypt-set-encryption-key
;;   Function: Set the encryption KEY, a string, for current buffer or optionally BUFFER.
;;
;;
;;; From the header of the crypt++.el package
;;;
;;; USAGE:
;;;
;;; By default, intended to be transparent.  User-defined variables
;;;
;;;	controlling ENCRYPTION are
;;;
;;;	   crypt-encryption-type
;;;	   crypt-encryption-file-extension
;;;	   crypt-never-ever-decrypt
;;;	   crypt-auto-write-buffer-encrypted
;;;	   crypt-confirm-password
;;;	   crypt-encrypted-disable-auto-save
;;;	   crypt-encryption-alist
;;;
;;;	controlling ENCODING are
;;;
;;;	   crypt-auto-decode-buffer
;;;	   crypt-auto-write-buffer
;;;	   crypt-query-if-interactive
;;;	   crypt-no-extension-implies-plain
;;;	   crypt-freeze-vs-fortran
;;;	   crypt-compact-vs-C++
;;;	   crypt-ignored-filenames
;;;	   crypt-default-encoding
;;;	   crypt-encoded-disable-auto-save
;;;	   crypt-encoding-alist
;;;
;;;	controlling file insertion are
;;;
;;;	   crypt-bind-insert-file
;;;	   crypt-auto-decode-insert
;;;
;;; To find out more about these variables, load this file, put your cursor at
;;; the end of any of the variable names, and hit C-h v [RET].
;;;
;;; NOTE: encryption users should set `crypt-encryption-type' to one of the
;;; values in `crypt-encryption-alist'
;;;
;;; Although rarely needed, the following functions may be called interactively
;;;
;;;	   (crypt-encoded-mode)
;;;	   (crypt-encode-region)
;;;	   (crypt-encrypted-mode)
;;;	   (crypt-encrypt-region)
;;;	   (crypt-set-encryption-key)
;;;	   (crypt-rebuild-tables)
;;;	   (crypt-insert-file)
;;;	   (crypt-bind-insert-file)
;;;	   (crypt-submit-report)
;;;
;;; To find out more about these functions, load this file, put your cursor
;;; inside any of the `()' of the above lines, and hit C-h f [RET].









auto-fill-function minor mode (indicator Fill):
Automatically break line at a previous space, in insertion of text.

Outline-Minor minor mode (indicator Outl):
Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode.

Outline mode:
Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:
C-c C-o C-n   outline-next-visible-heading	move by visible headings
C-c C-o C-p   outline-previous-visible-heading
C-c C-o C-f   outline-forward-same-level	similar but skip subheadings
C-c C-o C-b   outline-backward-same-level
C-c C-o C-u   outline-up-heading		    move from subheading to heading

M-x hide-body	make all text invisible (not headings).
M-x show-all	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
M-x hide-subtree   hide-subtree	make body and subheadings invisible.
M-x show-subtree   show-subtree	make body and subheadings visible.
M-x show-children   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
M-x hide-entry	   make immediately following body invisible.
M-x show-entry	   make it visible.
M-x hide-leaves	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
M-x show-branches  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.	The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil.











backward-ifdef		      (not bound to any keys)
  Function: Move point to beginning of the previous ifdef-endif.
down-ifdef		      (not bound to any keys)
  Function: Move point to beginning of nested ifdef or else-part.
forward-ifdef		      (not bound to any keys)
  Function: Move point to beginning of line of the next ifdef-endif.
hide-ifdef-block	      (not bound to any keys)
  Function: Hide the ifdef block (true or false part) enclosing or before the cursor.
hide-ifdef-define	      (not bound to any keys)
  Function: Define a VAR so that #ifdef VAR would be included.
hide-ifdef-mode		      (not bound to any keys)
  Function: Toggle Hide-Ifdef mode.  This is a minor mode, albeit a large one.
  Variable: Non-nil when hide-ifdef-mode is activated.
hide-ifdef-set-define-alist   (not bound to any keys)
  Function: Set the association for NAME to `hide-ifdef-env'.
hide-ifdef-toggle-outside-read-only (not bound to any keys)
  Function: Replacement for `toggle-read-only' within Hide Ifdef mode.
hide-ifdef-toggle-read-only   (not bound to any keys)
  Function: Toggle hide-ifdef-read-only.
hide-ifdef-undef	      (not bound to any keys)
  Function: Undefine a VAR so that #ifdef VAR would not be included.
hide-ifdef-use-define-alist   (not bound to any keys)
  Function: Set `hide-ifdef-env' to the define list specified by NAME.
hide-ifdefs		      (not bound to any keys)
  Function: Hide the contents of some #ifdefs.
next-ifdef		      (not bound to any keys)
  Function: Move to the beginning of the next #ifX, #else, or #endif.
previous-ifdef		      (not bound to any keys)
  Function: Move to the beginning of the previous #ifX, #else, or #endif.
show-ifdef-block	      (not bound to any keys)
  Function: Show the ifdef block (true or false part) enclosing or before the cursor.
show-ifdefs		      (not bound to any keys)
  Function: Cancel the effects of `hide-ifdef'.	 The contents of all #ifdefs is shown.
up-ifdef		      (not bound to any keys)
  Function: Move point to beginning of enclosing ifdef or else-part.







--- Don't use vc and crypt at the same time together within the emacs !!!

vc-cancel-version	      C-x v c
  Function: Get rid of most recently checked in version of this file.
vc-check-headers	      (not bound to any keys)
  Function: Check if the current file has any headers in it.
vc-clear-context	      (not bound to any keys)
  Function: Clear all cached file properties and the comment ring.
vc-comment-search-forward     (not bound to any keys)
  Function: Searches forwards through comment history for substring match.
vc-comment-search-reverse     (not bound to any keys)
  Function: Searches backwards through comment history for substring match.
vc-comment-to-change-log      (not bound to any keys)
  Function: Enter last VC comment into change log file for current buffer's file.
vc-create-snapshot	      C-x v s
  Function: Make a snapshot called NAME.
vc-diff			      C-x v =
  Function: Display diffs between file versions.
vc-directory		      C-x v d
  Function: Show version-control status of all files under the current directory.
vc-directory-18		      (not bound to any keys)
  Function: Show version-control status of all files under the current directory.
vc-ediff		      (not bound to any keys)
  Function: Run ediff on version REV of the current buffer in another window.
vc-finish-logentry	      (not bound to any keys)
  Function: Complete the operation implied by the current log entry.
vc-insert-headers	      C-x v h
  Function: Insert headers in a file for use with your version-control system.
vc-log-mode		      (not bound to any keys)
  Function: Minor mode for driving version-control tools.
vc-mode-line		      (not bound to any keys)
  Function: Set `vc-mode' to display type of version control for FILE.
vc-next-action		      C-x v v
  Function: Do the next logical checkin or checkout operation on the current file.
vc-next-comment		      (not bound to any keys)
  Function: Cycle forwards through comment history.
vc-previous-comment	      (not bound to any keys)
  Function: Cycle backwards through comment history.
vc-print-log		      C-x v l
  Function: List the change log of the current buffer in a window.
vc-register		      C-x v i
  Function: Register the current file into your version-control system.
vc-rename-file		      (not bound to any keys)
  Function: Rename file OLD to NEW, and rename its master file likewise.
vc-retrieve-snapshot	      C-x v r
  Function: Retrieve the snapshot called NAME.
vc-revert-buffer	      C-x v u
  Function: Revert the current buffer's file back to the latest checked-in version.
vc-revert-buffer1	      (not bound to any keys)
vc-steal-lock		      (not bound to any keys)
  Function: Steal the lock on the current workfile.
vc-toggle-read-only	      C-x C-q
  Function: Change read-only status of current buffer, perhaps via version control.
vc-update-change-log	      C-x v a
  Function: Find change log file and add entries from recent RCS logs.
vc-version-diff		      (not bound to any keys)
  Function: For FILE, report diffs between two stored versions REL1 and REL2 of it.
vc-version-other-window	      C-x v ~
  Function: Visit version REV of the current buffer in another window.





outline-backward-same-level   (not bound to any keys)
  Function: Move backward to the ARG'th subheading at same level as this one.
outline-forward-same-level    (not bound to any keys)
  Function: Move forward to the ARG'th subheading at same level as this one.
outline-minor-mode	      (not bound to any keys)
  Function: Toggle Outline minor mode.
  Variable: Non-nil if using Outline mode as a minor mode of some other mode.
outline-mode		      (not bound to any keys)
  Function: Set major mode for editing outlines with selective display.
outline-next-heading	      (not bound to any keys)
  Function: Move to the next (possibly invisible) heading line.
outline-next-visible-heading  (not bound to any keys)
  Function: Move to the next visible heading line.
outline-previous-visible-heading (not bound to any keys)
  Function: Move to the previous heading line.
outline-up-heading	      (not bound to any keys)
  Function: Move to the heading line of which the present line is a subheading.



quick-calc		      (not bound to any keys)
  Function: Do a quick calculation in the minibuffer without invoking full Calculator.



calendar		      menu-bar file calendar
  Function: Display a three-month calendar in another window.

M-x c	    calls the calendar and re-hilits it if possible
M-x d	    calls the diary, avoids buffer conflicts with the diary file.
	    With none or positive arguments this function behaves exactly
	    like the function "diary". With negative arguments you will be asked
	    for the date to start the diary, taking the absolute value of the
	    argument for the number of days to count. Example: (d -365),
	    1.1.1995 will give you the diary for the year 1995.


Calendar mode:
A major mode for the calendar window.

The commands for cursor movement are:

       C-f  one day forward	    C-b	 one day backward
       C-n  one week forward	    C-p	 one week backward
       M-}  one month forward	    M-{	 one month backward
       C-x ]  one year forward	    C-x [  one year backward
       C-a  beginning of week	    C-e	 end of week
       M-a  beginning of month	    M-e	 end of month
       M-<  beginning of year	    M->	 end of year

       g d  go to date

       g j  go to Julian date	    g a	 go to astronomical (Julian) day number
       g h  go to Hebrew date	    g i	 go to Islamic date
       g c  go to ISO date	    g f	 go to French Revolutionary date

       g m l  go to Mayan Long Count date
       g m n h	go to next occurrence of Mayan Haab date
       g m p h	go to previous occurrence of Mayan Haab date
       g m n t	go to next occurrence of Mayan Tzolkin date
       g m p t	go to previous occurrence of Mayan Tzolkin date
       g m n c	go to next occurrence of Mayan Calendar Round date
       g m p c	go to previous occurrence of Mayan Calendar Round date

You can mark a date in the calendar and switch the point and mark:

       C-@  mark date		      C-x C-x  exchange point and mark

You can determine the number of days (inclusive) between the point and mark by

       M-=  count days in the region

The commands for calendar movement are:

       C-x >  scroll one month right C-x <  scroll one month left
       M-v  scroll 3 months right    C-v  scroll 3 months left
       .  display current month	     o	display another month

Whenever it makes sense, the above commands take prefix arguments that
multiply their affect.	For convenience, the digit keys and the minus sign
are bound to digit-argument, so they need not be prefixed with ESC.

If the calendar window somehow becomes corrupted, it can be regenerated with

       C-c C-l	redraw the calendar

The following commands deal with holidays and other notable days:

       h  give holidays for the date specified by the cursor
       x  mark notable days
       u  unmark dates
       a  display notable days

The command M-x holidays causes the notable dates for the current month, and
the preceding and succeeding months, to be displayed, independently of the
calendar.

The following commands control the diary:

       m  mark diary entries	      u	 unmark dates
       d  display diary entries	      s	 show all diary entries
       M-x print-diary-entries	print diary entries

Displaying the diary entries causes the diary entries from the diary file
(for the date indicated by the cursor in the calendar window) to be
displayed in another window.  This function takes an integer argument that
specifies the number of days of calendar entries to be displayed, starting
with the date indicated by the cursor.

The command M-x print-diary-entries prints the diary buffer (as it appears)
on the line printer.

The command M-x diary causes the diary entries for the current date to be
displayed, independently of the calendar.  The number of days of entries is
governed by number-of-diary-entries. 

-- If M-x diary doesn't seem to do anything, try to search for your
diary buffer, i.g. with M-x M-b or M-x b diary-filename, where
diary-filename means the filename of your diary file defined in op-projects-ini.el.

The format of the entries in the diary file is described in the
documentation string for the variable `diary-file'.

When diary entries are in view in the window, they can be edited.  It is
important to keep in mind that the buffer displayed contains the entire
diary file, but with portions of it concealed from view.  This means, for
instance, that the forward-char command can put the cursor at what appears
to be the end of the line, but what is in reality the middle of some
concealed line.	 BE CAREFUL WHEN EDITING THE DIARY ENTRIES! (Inserting
additional lines or adding/deleting characters in the middle of a visible
line will not cause problems; watch out for end-of-line, however--it may
put you at the end of a concealed line far from where the cursor appears to
be!)  BEFORE EDITING THE DIARY IT IS BEST TO DISPLAY THE ENTIRE FILE WITH
show-all-diary-entries.	 BE SURE TO WRITE THE FILE BEFORE EXITING FROM THE
CALENDAR.

The following commands assist in making diary entries:

       i d  insert a diary entry for the selected date
       i w  insert a diary entry for the selected day of the week
       i m  insert a diary entry for the selected day of the month
       i y  insert a diary entry for the selected day of the year
       i b  insert a diary entry for the block days between point and mark
       i a  insert an anniversary diary entry for the selected date
       i c  insert a cyclic diary entry

There are corresponding commands to assist in making Hebrew- or Islamic-date
diary entries:

       i h d  insert a diary entry for the Hebrew date corresponding
		to the selected date
       i h m  insert a diary entry for the day of the Hebrew month
		corresponding to the selected day
       i h y  insert a diary entry for the day of the Hebrew year
		corresponding to the selected day
       i i d  insert a diary entry for the Islamic date corresponding
		to the selected date
       i i m  insert a diary entry for the day of the Islamic month
		corresponding to the selected day
       i i y  insert a diary entry for the day of the Islamic year
		corresponding to the selected day


*** SMW

Zum Ändern von Einträgen im Diary-File, wenn *Fancy diary entries*
offen sind: in das Fenster der *Fancy diary entries* an die zu
ändernde Stelle gehen, dann cde (change-diary-entries) tippen. Findet
dann sehr oft den eigentlichen Eintrag im Diary-File wieder.


All of the diary entry commands make nonmarking entries when given a prefix
argument; with no prefix argument, the diary entries are marking.

The day number in the year and the number of days remaining in the year can be
determined by

       p d  show day number and the number of days remaining in the year

Equivalent dates on the ISO commercial, Julian, Hebrew, Islamic, French
Revolutionary, and Mayan calendars can be determined by

       p c  show equivalent date on the ISO commercial calendar
       p j  show equivalent date on the Julian calendar
       p h  show equivalent date on the Hebrew calendar
       p i  show equivalent date on the Islamic calendar
       p f  show equivalent date on the French Revolutionary calendar
       p m  show equivalent date on the Mayan calendar

The astronomical (Julian) day number of a date is found with

       p a  show equivalent astronomical (Julian) day number

To find the times of sunrise and sunset and lunar phases use

       S  show times of sunrise and sunset
       M  show times of quarters of the moon

The times given apply to location `calendar-location-name' at latitude
`calendar-latitude', longitude `calendar-longitude'; set these variables for
your location.	The following variables are also consulted, and you must set
them if your system does not initialize them properly: `calendar-time-zone',
`calendar-daylight-time-offset', `calendar-standard-time-zone-name',
`calendar-daylight-time-zone-name', `calendar-daylight-savings-starts',
`calendar-daylight-savings-ends', `calendar-daylight-savings-starts-time',
`calendar-daylight-savings-ends-time'.

To exit from the calendar use

       q  exit from calendar

Set `view-diary-entries-initially' to a non-nil value to display
diary entries for the current date in
another window when the calendar is first displayed, if the current date is
visible.  The variable `number-of-diary-entries' controls number of days of
diary entries that to display initially or with the command M-x
diary.	For example, the default value 1 says to display only the current
day's diary entries.  The value 2 says to display both the
current day's and the next day's entries.

The value can also be a vector such as [0 2 2 2 2 4 1]; this value
says to display no diary entries on Sunday, the display the entries
for the current date and the day after on Monday through Thursday,
display Friday through Monday's entries on Friday, and display only
Saturday's entries on Saturday.

Set `view-calendar-holidays-initially' to a non-nil value to display
holidays for the current three month period on entry to the calendar.

Set `mark-diary-entries-in-calendar' to a non-nil value to mark in the
calendar all the dates that have diary entries.	 The variable
`diary-entry-marker' controls how to mark them.

The variable `calendar-load-hook', whose default value is nil, is list of
functions to be called when the calendar is first loaded.

The variable `initial-calendar-window-hook', whose default value is nil, is
list of functions to be called when the calendar window is first opened.  The
functions invoked are called after the calendar window is opened, but once
opened is never called again.  Leaving the calendar with the `q' command and
reentering it will cause these functions to be called again.

The variable `today-visible-calendar-hook', whose default value is nil, is the
list of functions called after the calendar buffer has been prepared with the
calendar when the current date is visible in the window.  This can be used,
for example, to replace today's date with asterisks; a function
calendar-star-date is included for this purpose:
 (setq today-visible-calendar-hook 'calendar-star-date)
It could also be used to mark the current date; a function is also provided
for this:
 (setq today-visible-calendar-hook 'calendar-mark-today)

The variable `today-invisible-calendar-hook', whose default value is nil, is
the list of functions called after the calendar buffer has been prepared with
the calendar when the current date is not visible in the window.

The variable `diary-display-hook' is the list of functions called after the
diary buffer is prepared.  The default value simply displays the diary file
using selective-display to conceal irrelevant diary entries.  An alternative
function `fancy-diary-display' is provided that, when used as the
`diary-display-hook', causes a noneditable buffer to be prepared with a neatly
organized day-by-day listing of relevant diary entries, together with any
known holidays.	 The inclusion of the holidays slows this fancy display of the
diary; to speed it up, set the variable `holidays-in-diary-buffer' to nil.

The variable `print-diary-entries-hook' is the list of functions called after
a temporary buffer is prepared with the diary entries currently visible in the
diary buffer.  The default value of this hook adds a heading (composed from
the diary buffer's mode line), does the printing with the command lpr-buffer,
and kills the temporary buffer.	 Other uses might include, for example,
rearranging the lines into order by day and time.

The Gregorian calendar is assumed.

Calendar mode:
A major mode for the calendar window.

For a complete description, type ? from within the calendar.

key             binding
---             -------

?		calendar-goto-info-node
i		Prefix Command
p		Prefix Command
s		show-all-diary-entries
D		view-other-diary-entries
d		view-diary-entries
m		mark-diary-entries
u		calendar-unmark
x		mark-calendar-holidays
h		calendar-cursor-holidays
a		list-calendar-holidays
q		exit-calendar
o		calendar-other-month
.		calendar-goto-today
C-c		Prefix Command
SPC		scroll-other-window
M		calendar-phases-of-moon
S		calendar-sunrise-sunset
g		Prefix Command
C-SPC		calendar-set-mark
C-@		calendar-set-mark
C-e		calendar-end-of-week
C-a		calendar-beginning-of-week
down		calendar-forward-week
right		calendar-forward-day
up		calendar-backward-week
left		calendar-backward-day
C-n		calendar-forward-week
C-f		calendar-forward-day
C-p		calendar-backward-week
C-b		calendar-backward-day
C-v		scroll-calendar-left-three-months
next		scroll-calendar-left-three-months
ESC		Prefix Command
prior		scroll-calendar-right-three-months
C-x		Prefix Command
-		negative-argument
S-delete	calendar-not-implemented
C-w		calendar-not-implemented
9		digit-argument
8		digit-argument
7		digit-argument
6		digit-argument
5		digit-argument
4		digit-argument
3		digit-argument
2		digit-argument
1		digit-argument
0		digit-argument
C-down-mouse-3	Prefix Command
down-mouse-3	Prefix Command
mouse-2		ignore
down-mouse-2	calendar-mouse-2-date-menu
menu-bar	Prefix Command

i i		Prefix Command
i h		Prefix Command
i c		insert-cyclic-diary-entry
i b		insert-block-diary-entry
i a		insert-anniversary-diary-entry
i y		insert-yearly-diary-entry
i m		insert-monthly-diary-entry
i w		insert-weekly-diary-entry
i d		insert-diary-entry

p m		calendar-print-mayan-date
p f		calendar-print-french-date
p i		calendar-print-islamic-date
p h		calendar-print-hebrew-date
p a		calendar-print-astro-day-number
p j		calendar-print-julian-date
p c		calendar-print-iso-date
p d		calendar-print-day-of-year

C-c C-l		redraw-calendar

g m		Prefix Command
g f		calendar-goto-french-date
g c		calendar-goto-iso-date
g i		calendar-goto-islamic-date
g h		calendar-goto-hebrew-date
g a		calendar-goto-astro-day-number
g j		calendar-goto-julian-date
g d		calendar-goto-date

ESC =		calendar-count-days-region
ESC >		calendar-end-of-year
ESC <		calendar-beginning-of-year
ESC e		calendar-end-of-month
ESC a		calendar-beginning-of-month
ESC }		calendar-forward-month
ESC {		calendar-backward-month
ESC v		scroll-calendar-right-three-months

C-x C-x		calendar-exchange-point-and-mark
C-x ]		calendar-forward-year
C-x [		calendar-backward-year
C-x <		scroll-calendar-left
C-x >		scroll-calendar-right

C-down-mouse-3 scroll-forward scroll-calendar-left-three-months
C-down-mouse-3 scroll-backward scroll-calendar-right-three-months
C-down-mouse-3 mark-diary-entries mark-diary-entries
C-down-mouse-3 list-holidays list-calendar-holidays
C-down-mouse-3 mark-holidays mark-calendar-holidays
C-down-mouse-3 unmark calendar-unmark
C-down-mouse-3 lunar-phases calendar-phases-of-moon
C-down-mouse-3 show-diary show-all-diary-entries
C-down-mouse-3 exit-calendar exit-calendar

i i y		insert-yearly-islamic-diary-entry
i i m		insert-monthly-islamic-diary-entry
i i d		insert-islamic-diary-entry

i h y		insert-yearly-hebrew-diary-entry
i h m		insert-monthly-hebrew-diary-entry
i h d		insert-hebrew-diary-entry

g m n		Prefix Command
g m p		Prefix Command
g m l		calendar-goto-mayan-long-count-date

g m n t		calendar-next-tzolkin-date
g m n h		calendar-next-haab-date
g m n c		calendar-next-calendar-round-date

g m p t		calendar-previous-tzolkin-date
g m p h		calendar-previous-haab-date
g m p c		calendar-previous-calendar-round-date




SMW: diary-file's value is "~/op-projects/op-diary", 
	     defined for the project-mode in the file  op-project.ini.el



Documentation:
*Name of the file in which one's personal diary of dates is kept.

The file's entries are lines in any of the forms

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

at the beginning of the line; the remainder of the line is the diary entry
string for that date.  MONTH and DAY are one or two digit numbers, YEAR is
a number and may be written in full or abbreviated to the final two digits.
If the date does not contain a year, it is generic and applies to any year.
DAYNAME entries apply to any date on which is on that day of the week.
MONTHNAME and DAYNAME can be spelled in full, abbreviated to three
characters (with or without a period), capitalized or not.  Any of DAY,
MONTH, or MONTHNAME, YEAR can be `*' which matches any day, month, or year,
respectively.

The European style (in which the day precedes the month) can be used
instead, if you execute `european-calendar' when in the calendar, or set
`european-calendar-style' to t in your .emacs file.  The European forms are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

To revert to the default American style from the European style, execute
`american-calendar' in the calendar.

A diary entry can be preceded by the character
`diary-nonmarking-symbol' (ordinarily `&') to make that entry
nonmarking--that is, it will not be marked on dates in the calendar
window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first with
either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample diary
entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day name with
no trailing blanks or punctuation, then that line is not displayed in the
diary window; only the continuation lines is shown.  For example, the
single diary entry

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

will appear in the diary window without the date line at the beginning.  This
facility allows the diary window to look neater, but can cause confusion if
used with more than one day's entries displayed.

Diary entries can be based on Lisp sexps.  For example, the diary entry

      %%(diary-block 11 1 1990 11 10 1990) Vacation

causes the diary entry "Vacation" to appear from November 1 through November
10, 1990.  Other functions available are `diary-float', `diary-anniversary',
`diary-cyclic', `diary-day-of-year', `diary-iso-date', `diary-french-date',
`diary-hebrew-date', `diary-islamic-date', `diary-mayan-date',
`diary-yahrzeit', `diary-sunrise-sunset', `diary-phases-of-moon',
`diary-parasha', `diary-omer', `diary-rosh-hodesh', and
`diary-sabbath-candles'.  See the documentation for the function
`list-sexp-diary-entries' for more details.

Diary entries based on the Hebrew and/or the Islamic calendar are also
possible, but because these are somewhat slow, they are ignored
unless you set the `nongregorian-diary-listing-hook' and the
`nongregorian-diary-marking-hook' appropriately.  See the documentation
for these functions for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'.


Dokumentation zu (defun list-sexp-diary-entries (date) aus diary.el:
  Add sexp entries for DATE from the diary file to `diary-entries-list'.
  Also, Make them visible in the diary file.  Returns t if any entries were
  found.

Sexp diary entries must be prefaced by a `sexp-diary-entry-symbol' (normally
`%%').  The form of a sexp diary entry is

                  %%(SEXP) ENTRY

Both ENTRY and DATE are globally available when the SEXP is evaluated.  If the
SEXP yields the value nil, the diary entry does not apply.  If it yields a
non-nil value, ENTRY will be taken to apply to DATE; if the non-nil value is a
string, that string will be the diary entry in the fancy diary display.

For example, the following diary entry will apply to the 21st of the month
if it is a weekday and the Friday before if the 21st is on a weekend:

      &%%(let ((dayname (calendar-day-of-week date))
               (day (extract-calendar-day date)))
           (or
             (and (= day 21) (memq dayname '(1 2 3 4 5)))
             (and (memq day '(19 20)) (= dayname 5)))
         ) UIUC pay checks deposited

A number of built-in functions are available for this type of diary entry:

      %%(diary-float MONTH DAYNAME N) text
                  Entry will appear on the Nth DAYNAME of MONTH.
                  (DAYNAME=0 means Sunday, 1 means Monday, and so on;
                  if N is negative it counts backward from the end of
                  the month.  MONTH can be a list of months, a single
                  month, or t to specify all months.

      %%(diary-block M1 D1 Y1 M2 D2 Y2) text
                  Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
                  inclusive.  (If `european-calendar-style' is t, the
                  order of the parameters should be changed to D1, M1, Y1,
                  D2, M2, Y2.)

      %%(diary-anniversary MONTH DAY YEAR) text
                  Entry will appear on anniversary dates of MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of years since the MONTH DAY, YEAR and %s will be replaced
                  by the ordinal ending of that number (that is, `st', `nd',
                  `rd' or `th', as appropriate.  The anniversary of February
                  29 is considered to be March 1 in a non-leap year.

      %%(diary-cyclic N MONTH DAY YEAR) text
                  Entry will appear every N days, starting MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to N, DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of repetitions since the MONTH DAY, YEAR and %s will
                  be replaced by the ordinal ending of that number (that is,
                  `st', `nd', `rd' or `th', as appropriate.

      %%(diary-day-of-year)
                  Diary entries giving the day of the year and the number of
                  days remaining in the year will be made every day.  Note
                  that since there is no text, it makes sense only if the
                  fancy diary display is used.

      %%(diary-iso-date)
                  Diary entries giving the corresponding ISO commercial date
                  will be made every day.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.

      %%(diary-french-date)
                  Diary entries giving the corresponding French Revolutionary
                  date will be made every day.  Note that since there is no
                  text, it makes sense only if the fancy diary display is used.

      %%(diary-islamic-date)
                  Diary entries giving the corresponding Islamic date will be
                  made every day.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-hebrew-date)
                  Diary entries giving the corresponding Hebrew date will be
                  made every day.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-astro-day-number) Diary entries giving the corresponding
                  astronomical (Julian) day number will be made every day.
                  Note that since there is no text, it makes sense only if the
                  fancy diary display is used.

      %%(diary-julian-date) Diary entries giving the corresponding
                 Julian date will be made every day.  Note that since
                 there is no text, it makes sense only if the fancy diary
                 display is used.

      %%(diary-sunrise-sunset)
                  Diary entries giving the local times of sunrise and sunset
                  will be made every day.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.
                  Floating point required.

      %%(diary-phases-of-moon)
                  Diary entries giving the times of the phases of the moon
                  will be when appropriate.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.
                  Floating point required.

      %%(diary-yahrzeit MONTH DAY YEAR) text
                  Text is assumed to be the name of the person; the date is
                  the date of death on the *civil* calendar.  The diary entry
                  will appear on the proper Hebrew-date anniversary and on the
                  day before.  (If `european-calendar-style' is t, the order
                  of the parameters should be changed to DAY, MONTH, YEAR.)
                  
      %%(diary-rosh-hodesh)
                  Diary entries will be made on the dates of Rosh Hodesh on
                  the Hebrew calendar.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-parasha)
                  Diary entries giving the weekly parasha will be made on
                  every Saturday.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-omer)
                  Diary entries giving the omer count will be made every day
                  from Passover to Shavuoth.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.

Marking these entries is *extremely* time consuming, so these entries are
best if they are nonmarking.





   The generality of sexp diary entries lets you specify any diary entry
that you can describe algorithmically.  Suppose you get paid on the 21st
of the month if it is a weekday, and to the Friday before if the 21st is
on a weekend.  The diary entry

     &%%(let ((dayname (calendar-day-of-week date))
              (day (car (cdr date))))
           (or (and (= day 21) (memq dayname '(1 2 3 4 5)))
               (and (memq day '(19 20)) (= dayname 5)))
              ) Pay check deposited

applies to just those dates.  This example illustrates how the sexp can
depend on the variable `date'; this variable is a list (MONTH DAY YEAR)
that gives the Gregorian date for which the diary entries are being
found.  If the value of the expression is `t', the entry applies to
that date.  If the expression evaluates to `nil', the entry does *not*
apply to that date.



SMW Zusätzlichkeiten:

Ein Gruppenkalender ist verfügbar. Definiert kann dieser werden in der
Variablen

  group-diary-file's value is "~/op-projects/op-group-diary", 
	     defined for the project-mode in the file  op-project.ini.el

Eintragen der Termine in den Gruppen-Tagebuch-File:
 i g c		insert-group-cyclic-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g b		insert-group-block-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g a		insert-group-anniversary-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g y		insert-group-yearly-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g m		insert-group-monthly-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g w		insert-group-weekly-diary-entry (Funktion steht nicht interaktiv zur Verfügung)
 i g d		insert-group-diary-entry (Funktion steht nicht interaktiv zur Verfügung)

Ansehen des Gesamt-Kalenders im Calender-Mode mit d,
Ansehen nur des Gruppenkalenders mit e,
Ansehen nur des eigenen Kalenders mit r,
Ansehen des eigenen Files mit s,
Ansehen des Gruppenfiles mit g.

Im eigenen Kalender sollte mit   #include "Pfad/Gruppenkalender"   der Gruppenkalender
eingebunden sein.







Calculator modes:
M-x calc (complicated, but very very good)

M-x quick-calc (fast and plug and play)
