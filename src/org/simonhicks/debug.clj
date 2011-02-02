(ns org.simonhicks.debug
  (:import [clojure.lang RT]
           [java.lang StringBuilder]
           [java.io LineNumberReader PushbackReader])
  (:use [clojure.contrib.io :only (reader)]
        [clojure.contrib.str-utils :only (re-gsub)]))

(defmacro db 
  "prepend this to a form to see what it evaluates to each time it is executed.
  Example:

  user=> (for [i (range 3)] (db + i 1))
  (+ i 1) ; => 1
  (+ i 1) ; => 2
  (+ i 1) ; => 3
  => (1 2 3)

  you can also use it with a message string that will be used in place of the s-expr

  also see (doc pk) and (doc mg) for similar
  "
  [& more] 
  (let [[msg & form] (if (and (string? (first more)) (> 1 (count more)))
                       more
                       (cons (str more) more))]
    `(let [result# ~form]
       (println (str ~msg " ; => " result#))
       result#)))

(defmacro db+
  "When prepended to a form, db+ prints the unevaluated form, followed 
  by the form with args evaluated, and the result, and then returns the 
  result so the encompassing program can continue as normal.
  Example:

  user=> (for [i (range 3)] (db+ i 1))
  (+ i 1) => (+ 0 1) ; => 1
  (+ i 1) => (+ 1 1) ; => 2
  (+ i 1) => (+ 2 1) ; => 3
  => (1 2 3)

  see also db ct and cl for similar debugging tools
  "
  [& more]
  (let [[msg & form] (if (and (string? (first more)) (> 1 (count more)))
                       more
                       (cons (str more) more))]
    `(let [[fun# & args#] ~(vec form)
           f-name# (if (fn? fun#) (:name (meta fun#)) fun#)
           result# ~form]
       (println (str ~msg " => " (str (list* f-name# args#)) " ; => " result#))
       result#)))

(defmacro mg
  "When prepended to a form along with a msg-fn, mg prints the unevaluated form, 
  followed by the result of passing the evaluated form to the msg-fn, finally 
  returning the result of the evaluated form. an example of this is 
  org.simonhicks.debug/cl

  (defmacro cl
    [& more]
    `(mg (fn [c#] (re-gsub #\"class \" \"\" (str (class c#)))) ~@more))
  "
  [msg-fn & form] 
   `(let [result# ~form]
       (println (str (quote ~form) " ; => " (~msg-fn result#)))
       result#))

(defmacro ct
  "When prepended to a form, ct prints the unevaluated form, and then evaluates 
  the form and prints the number of items in the resulting collection, finally
  returning the result of the evaluation.
  Exmaple:

  user=> (ct range 1 10)
  (range 1 10) ; => 9 items
  => (1 2 3 4 5 6 7 8 9)
  "
  [& more]
  `(mg (fn [c#] (str (count c#) " items")) ~@more))

(defmacro cl
  "When prepended to a form, cl prints the unevaluated form, and then evaluated
  the form and prints the class ofthe result. It returns the result of the 
  evaluation.
  Example:

  user=> (cl vec (range 1 10))
  (vec (range 1 10) ; => clojure.lang.PersistentVector
  => [1 2 3 4 5 6 7 8 9]
  "
  [& more]
  `(mg (fn [c#] (re-gsub #"class " "" (str (class c#)))) ~@more))

(defn get-src-str
  "Returns the source for whatever is bound to sym as a string if it can be found"
  [sym]
  (let [{:keys [file line]} (eval `(meta (var ~sym)))]
    (when-let [strm (.getResourceAsStream (RT/baseLoader) file)]
      (let [lrdr (LineNumberReader. (reader strm))
            strb (StringBuilder.)]
        (dotimes [_ (dec line)] (.readLine lrdr))
        (let [rdr (proxy [PushbackReader] [lrdr]
                    (read [] (let [i (proxy-super read)]
                               (.append strb (char i))
                               i)))]
          (read (PushbackReader. rdr))
          (str strb))))))

(defn get-src
  "Returns the source for whatever is bound to sym if it can be found. 
  NB. this returns the code itself, not a string. If you want a string,
  use get-src-str or src"
  [sym]
  (let [{:keys [file line]} (eval `(meta (var ~sym)))]
    (when-let [strm (.getResourceAsStream (RT/baseLoader) file)]
      (let [lrdr (LineNumberReader. (reader strm))]
        (dotimes [_ (dec line)] (.readLine lrdr))
        (read (PushbackReader. lrdr))))))

(defmacro src
  "Prints the source of whatever is bound to sym if it can be found."
  [sym]
  `(println (get-src-str '~sym)))
