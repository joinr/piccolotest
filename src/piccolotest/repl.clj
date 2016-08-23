;;provides a wrapped clojure
;;repl in compatible swing node form.
(ns piccolotest.repl
  (:require [org.dipert.swingrepl.main]
            [spork.util [clipboard :as clip]]
            [spork.cljgui.components [swing :as swing]])
  (:import [java.awt.event KeyEvent]
           [java.awt Robot]))

;;so....o
(comment 
(def repl-in (clojure.java.io/reader))
(def strokes (atom []))
(defn spy [obj]
  (doto obj 
    (.addKeyListener  
     (reify java.awt.event.KeyListener
       (^void keyTyped [this ^KeyEvent e]
         (swap! strokes conj e))
       (^void keyPressed [this ^KeyEvent e])
       (^void keyReleased [this ^KeyEvent e]
         ;(swap! strokes conj e)
         )))))
(def txt (spork.cljgui.components.swing/text-field ""))
)

;;this is what jconsole does.
;;if you don't provide an in, it'll create
;;a pipedwriter, and then wrap it with a
;;pipedreader.
		;; inPipe = cin;
		;; if (inPipe == null) {
		;; 	PipedWriter pout = new PipedWriter();
		;; 	out = new PrintWriter(pout);
		;; 	try {
		;; 		inPipe = new PipedReader(pout);
		;; 	} catch (IOException e) {
		;; 		print("Console internal error: " + e);
		;; 	}
		;; }

;;Present the string as if the user entered
;;a sequence of key-presses
(defn repl-panel
  ([opts]
   (org.dipert.swingrepl.main/make-repl-jconsole
    (merge org.dipert.swingrepl.main/default-opts opts)))
  ([w h ]
   (doto (repl-panel {})
         (.setPreferredSize (java.awt.Dimension. w h)))))

;; (def repl-input (java.io.PipedWriter.))
;; (def repl-in    (java.io.PipedReader. repl-input))

              
(comment 
(defn paste-repl! [^String xs]  
  (clip/paste! xs)
  (doto (Robot.)
    (.keyPress   KeyEvent/VK_CONTROL)
    (.keyPress   KeyEvent/VK_V)
    (.keyRelease KeyEvent/VK_V)
    (.keyRelease KeyEvent/VK_CONTROL)
    ))

)

(defn send-repl [^bsh.util.JConsole rpl ^String xs]
  (do (.readLine rpl xs)
      (.print rpl xs)
      (.enterEvent rpl)))

(defmacro eval-repl [rpl & body]
  (let [r (with-meta (gensym "swingrepl") {:tag 'bsh.util.JConsole})]
    `(let [~r ~rpl]
       (send-repl ~r
                  ~(str (first body))))))
      
(def codes
  '[KeyEvent/VK_0 \0
    KeyEvent/VK_1 \1
    KeyEvent/VK_2 \2
    KeyEvent/VK_3 \3
    KeyEvent/VK_4 \4
    KeyEvent/VK_5 \5
    KeyEvent/VK_6 \6
    KeyEvent/VK_7 \7
    KeyEvent/VK_8 \8
    KeyEvent/VK_9 \9
    KeyEvent/VK_A \A
    KeyEvent/VK_ADD \+
    KeyEvent/VK_ALT :alt
    KeyEvent/VK_AMPERSAND \&
    KeyEvent/VK_ASTERISK \*
    KeyEvent/VK_AT \@
    KeyEvent/VK_B \B
    KeyEvent/VK_BACK_QUOTE \`
    KeyEvent/VK_BACK_SLASH \\
    KeyEvent/VK_BACK_SPACE :back-space 
    KeyEvent/VK_BRACELEFT \{
    KeyEvent/VK_BRACERIGHT \} 
    KeyEvent/VK_C \C
    KeyEvent/VK_CAPS_LOCK :caps-lock 
    KeyEvent/VK_CLOSE_BRACKET \]
    KeyEvent/VK_COLON \:
    KeyEvent/VK_COMMA \, 
    KeyEvent/VK_CONTROL :control
    KeyEvent/VK_COPY :copy
    KeyEvent/VK_CUT  :cut
    KeyEvent/VK_D \D
    KeyEvent/VK_DEAD_ABOVEDOT DEAD_ABOVEDOT
    KeyEvent/VK_DEAD_ABOVERING DEAD_ABOVERING
    KeyEvent/VK_DEAD_ACUTE DEAD_ACUTE
    KeyEvent/VK_DEAD_BREVE DEAD_BREVE
    KeyEvent/VK_DEAD_CARON DEAD_CARON
    KeyEvent/VK_DEAD_CEDILLA DEAD_CEDILLA
    KeyEvent/VK_DEAD_CIRCUMFLEX DEAD_CIRCUMFLEX
    KeyEvent/VK_DEAD_DIAERESIS DEAD_DIAERESIS
    KeyEvent/VK_DEAD_DOUBLEACUTE DEAD_DOUBLEACUTE
    KeyEvent/VK_DEAD_GRAVE DEAD_GRAVE
    KeyEvent/VK_DEAD_IOTA DEAD_IOTA
    KeyEvent/VK_DEAD_MACRON DEAD_MACRON
    KeyEvent/VK_DEAD_OGONEK DEAD_OGONEK
    KeyEvent/VK_DEAD_SEMIVOICED_SOUND DEAD_SEMIVOICED_SOUND
    KeyEvent/VK_DEAD_TILDE DEAD_TILDE
    KeyEvent/VK_DEAD_VOICED_SOUND DEAD_VOICED_SOUND
    KeyEvent/VK_DECIMAL DECIMAL
    KeyEvent/VK_DELETE DELETE
    KeyEvent/VK_DIVIDE DIVIDE
    KeyEvent/VK_DOLLAR DOLLAR
    KeyEvent/VK_DOWN DOWN
    KeyEvent/VK_E E
    KeyEvent/VK_END END
    KeyEvent/VK_ENTER ENTER
    KeyEvent/VK_EQUALS EQUALS
    KeyEvent/VK_ESCAPE ESCAPE
    KeyEvent/VK_EURO_SIGN EURO_SIGN
    KeyEvent/VK_EXCLAMATION_MARK EXCLAMATION_MARK
    KeyEvent/VK_F F
    KeyEvent/VK_F1 F1
    KeyEvent/VK_F10 F10
    KeyEvent/VK_F11 F11
    KeyEvent/VK_F12 F12
    KeyEvent/VK_F13 F13
    KeyEvent/VK_F14 F14
    KeyEvent/VK_F15 F15
    KeyEvent/VK_F16 F16
    KeyEvent/VK_F17 F17
    KeyEvent/VK_F18 F18
    KeyEvent/VK_F19 F19
    KeyEvent/VK_F2 F2
    KeyEvent/VK_F20 F20
    KeyEvent/VK_F21 F21
    KeyEvent/VK_F22 F22
    KeyEvent/VK_F23 F23
    KeyEvent/VK_F24 F24
    KeyEvent/VK_F3 F3
    KeyEvent/VK_F4 F4
    KeyEvent/VK_F5 F5
    KeyEvent/VK_F6 F6
    KeyEvent/VK_F7 F7
    KeyEvent/VK_F8 F8
    KeyEvent/VK_F9 F9
    KeyEvent/VK_FINAL FINAL
    KeyEvent/VK_FIND FIND
    KeyEvent/VK_FULL_WIDTH FULL_WIDTH
    KeyEvent/VK_G G
    KeyEvent/VK_GREATER GREATER
    KeyEvent/VK_H H
    KeyEvent/VK_HALF_WIDTH HALF_WIDTH
    KeyEvent/VK_HELP HELP
    KeyEvent/VK_HIRAGANA HIRAGANA
    KeyEvent/VK_HOME HOME
    KeyEvent/VK_I I
    KeyEvent/VK_INPUT_METHOD_ON_OFF INPUT_METHOD_ON_OFF
    KeyEvent/VK_INSERT INSERT
    KeyEvent/VK_INVERTED_EXCLAMATION_MARK INVERTED_EXCLAMATION_MARK
    KeyEvent/VK_J J
    KeyEvent/VK_JAPANESE_HIRAGANA JAPANESE_HIRAGANA
    KeyEvent/VK_JAPANESE_KATAKANA JAPANESE_KATAKANA
    KeyEvent/VK_JAPANESE_ROMAN JAPANESE_ROMAN
    KeyEvent/VK_K K
    KeyEvent/VK_KANA KANA
    KeyEvent/VK_KANA_LOCK KANA_LOCK
    KeyEvent/VK_KANJI KANJI
    KeyEvent/VK_KATAKANA KATAKANA
    KeyEvent/VK_KP_DOWN KP_DOWN
    KeyEvent/VK_KP_LEFT KP_LEFT
    KeyEvent/VK_KP_RIGHT KP_RIGHT
    KeyEvent/VK_KP_UP KP_UP
    KeyEvent/VK_L L
    KeyEvent/VK_LEFT LEFT
    KeyEvent/VK_LEFT_PARENTHESIS LEFT_PARENTHESIS
    KeyEvent/VK_LESS LESS
    KeyEvent/VK_M M
    KeyEvent/VK_META META
    KeyEvent/VK_MINUS MINUS
    KeyEvent/VK_MODECHANGE MODECHANGE
    KeyEvent/VK_MULTIPLY MULTIPLY
    KeyEvent/VK_N N
    KeyEvent/VK_NONCONVERT NONCONVERT
    KeyEvent/VK_NUMBER_SIGN NUMBER_SIGN
    KeyEvent/VK_NUMPAD0 NUMPAD0
    KeyEvent/VK_NUMPAD1 NUMPAD1
    KeyEvent/VK_NUMPAD2 NUMPAD2
    KeyEvent/VK_NUMPAD3 NUMPAD3
    KeyEvent/VK_NUMPAD4 NUMPAD4
    KeyEvent/VK_NUMPAD5 NUMPAD5
    KeyEvent/VK_NUMPAD6 NUMPAD6
    KeyEvent/VK_NUMPAD7 NUMPAD7
    KeyEvent/VK_NUMPAD8 NUMPAD8
    KeyEvent/VK_NUMPAD9 NUMPAD9
    KeyEvent/VK_NUM_LOCK NUM_LOCK
    KeyEvent/VK_O O
    KeyEvent/VK_OPEN_BRACKET OPEN_BRACKET
    KeyEvent/VK_P P
    KeyEvent/VK_PAGE_DOWN PAGE_DOWN
    KeyEvent/VK_PAGE_UP PAGE_UP
    KeyEvent/VK_PASTE PASTE
    KeyEvent/VK_PAUSE PAUSE
    KeyEvent/VK_PERIOD PERIOD
    KeyEvent/VK_PLUS PLUS
    KeyEvent/VK_PREVIOUS_CANDIDATE PREVIOUS_CANDIDATE
    KeyEvent/VK_PRINTSCREEN PRINTSCREEN
    KeyEvent/VK_PROPS PROPS
    KeyEvent/VK_Q Q
    KeyEvent/VK_QUOTE QUOTE
    KeyEvent/VK_QUOTEDBL QUOTEDBL
    KeyEvent/VK_R R
    KeyEvent/VK_RIGHT RIGHT
    KeyEvent/VK_RIGHT_PARENTHESIS RIGHT_PARENTHESIS
    KeyEvent/VK_ROMAN_CHARACTERS ROMAN_CHARACTERS
    KeyEvent/VK_S S
    KeyEvent/VK_SCROLL_LOCK SCROLL_LOCK
    KeyEvent/VK_SEMICOLON SEMICOLON
    KeyEvent/VK_SEPARATER SEPARATER
    KeyEvent/VK_SEPARATOR SEPARATOR
    KeyEvent/VK_SHIFT SHIFT
    KeyEvent/VK_SLASH SLASH
    KeyEvent/VK_SPACE SPACE
    KeyEvent/VK_STOP STOP
    KeyEvent/VK_SUBTRACT SUBTRACT
    KeyEvent/VK_T T
    KeyEvent/VK_TAB TAB
    KeyEvent/VK_U U
    KeyEvent/VK_UNDEFINED UNDEFINED
    KeyEvent/VK_UNDERSCORE UNDERSCORE
    KeyEvent/VK_UNDO UNDO
    KeyEvent/VK_UP UP
    KeyEvent/VK_V V
    KeyEvent/VK_W W
    KeyEvent/VK_WINDOWS WINDOWS
    KeyEvent/VK_X X
    KeyEvent/VK_Y Y
    KeyEvent/VK_Z Z])
                

;;So...our good friends at beanshell, in all their OOP glory,
;;decided to make JConsole hide its REALLY useful "type" method,
;;which allows us to programatically push text to the
;;widget.
(comment
(defn make-repl-jframe
  "Displays a JFrame with JConsole and attached REPL."
  ([] (make-repl-jframe {}))
  ([optmap]
     (let [options (merge default-opts optmap)
           {:keys [title width height font on-close prompt init eval]} options
           jframe (doto (JFrame. title)
                   (.setSize width height)
                   (.setDefaultCloseOperation on-close)
                   (.setLocationRelativeTo nil))
           eof (window-closing-dispatcher jframe)]
       (let [console (make-repl-jconsole
                      (merge
                       {:eof eof}
                       options))]
         (doto (.getContentPane jframe)
           (.setLayout (java.awt.BorderLayout.))
           (.add console))
         (doto jframe
           (.pack)
           (.setSize width height))
         (.requestFocus console)
         (.setVisible jframe true)))))
  
(ns org.dipert.swingrepl.main
  "Swing Clojure REPL using BeanShell's JConsole"
  (:require clojure.main clojure.repl)
  (:import (javax.swing JFrame)
           (java.awt.event WindowEvent)
           (java.awt Font)
           (bsh.util JConsole))
  (:gen-class))

(def ^{:doc "Formatted Clojure version string"
       :private true}
     clj-version
     (apply str (interpose \. (map *clojure-version* [:major :minor :incremental]))))

(def ^{:doc "Default REPL options"
       :private false}
     default-opts
     {:width 972
      :height 400
      :font (Font. "Monospaced" Font/PLAIN 14)
      :title (str "Clojure " clj-version " REPL")
      :prompt #(printf "%s=> " (ns-name *ns*))
      :init #()
      :eval eval
      :on-close JFrame/DISPOSE_ON_CLOSE})

(def ^{:doc "Default debug REPL options"
       :private false}
     default-dbg-opts
     {:title (str "Clojure " clj-version " Debug REPL")
      :prompt #(print "dr => ")
      :eval (comment "See make-dbg-repl-jframe")})

(defn- make-repl-thread [console & repl-args]
  (binding [*out* (.getOut console)
            *in*  (clojure.lang.LineNumberingPushbackReader. (.getIn console))
            *err* (.getOut console)]
    (Thread. (bound-fn []
               (apply clojure.main/repl repl-args)))))

(defn- window-closing-dispatcher [window]
  #(.dispatchEvent window (WindowEvent. window WindowEvent/WINDOW_CLOSING)))


(defn make-repl-jconsole
  "Returns a JConsole component"
  [options]
  (let [{:keys [font prompt init eval eof]} options
        console (bsh.util.JConsole. font)
        thread (make-repl-thread console :prompt prompt :init init :eval eval)
        stopper (clojure.repl/thread-stopper thread)]
    (doto console
      (.setInterruptFunction (fn [reason] (stopper reason)))
      (.setEOFFunction eof))
    (.start thread)
    console))


(defn make-repl-jframe
  "Displays a JFrame with JConsole and attached REPL."
  ([] (make-repl-jframe {}))
  ([optmap]
     (let [options (merge default-opts optmap)
           {:keys [title width height font on-close prompt init eval]} options
           jframe (doto (JFrame. title)
                   (.setSize width height)
                   (.setDefaultCloseOperation on-close)
                   (.setLocationRelativeTo nil))
           eof (window-closing-dispatcher jframe)]
       (let [console (make-repl-jconsole
                      (merge
                       {:eof eof}
                       options))]
         (doto (.getContentPane jframe)
           (.setLayout (java.awt.BorderLayout.))
           (.add console))
         (doto jframe
           (.pack)
           (.setSize width height))
         (.requestFocus console)
         (.setVisible jframe true)))))

;; local-bindings and eval-with-locals are from http://gist.github.com/252421
;; Inspired by George Jahad's version: http://georgejahad.com/clojure/debug-repl.html
(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (map key @clojure.lang.Compiler/LOCAL_ENV)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(declare ^:dynamic *locals*)
(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
  values."
  [locals form]
  (binding [*locals* locals]
    (eval
      `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
         ~form))))

(defmacro make-dbg-repl-jframe
  "Displays a JFrame with JConsole and attached REPL. The frame has the context
  from wherever it has been called, effectively creating a debugging REPL.
  Usage:
    (use 'org.dipert.swingrepl.main)
    (defn foo [a] (+ a 5) (make-dbg-repl-jframe {}) (+ a 2))
    (foo 3)
  This will pop up the debugging REPL, you should be able to access the var 'a'
  from the REPL."
  ([] `(make-dbg-repl-jframe {}))
  ([optmap]
   `(make-repl-jframe (merge
      default-opts
      default-dbg-opts
      {:eval (partial eval-with-locals (local-bindings))}
      ~optmap))))

(defn -main
  [& args]
(make-repl-jframe {:on-close JFrame/EXIT_ON_CLOSE}))
)
