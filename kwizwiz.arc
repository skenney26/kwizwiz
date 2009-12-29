
; Sean
; skenney26@gmail.com

(= question-id* 0 questions* (table))

(deftem question
  id 0 url "" q "" a "" b "" c "" d "")

(def between (x y (o str))
  (if str
      (withs (n  (len x)
              p1 (+ (posmatch x str) n)
              p2 (posmatch y str p1))
        (cut str p1 p2))
      (+ x (rand (+ (- y x) 1)))))

(def video-url (url)
  (+ "http://www.youtube.com/v/"
     (between "=" "&" url)
     "&hl=en_US&fs=1&showinfo=0&autoplay=1"))

(mac ret (var val . body)
 `(let ,var ,val ,@body ,var))

(def question (url q a b c d)
  (let id (++ question-id*)
    (ret new (inst 'question
                   'id id 'url (video-url url)
                   'q q 'a a 'b b 'c c 'd d)
      (= (questions* id) new))))
      
(question "http://www.youtube.com/watch?v=XX3A3xE3dj8&feature=related"
          "What martial art is this?"
          "Judo"
          "Taekwondo"
          "Karate"
          "Muay Thai")
      
(question "http://www.youtube.com/watch?v=VFZorStky7U"
          "This rock icon once told a friend that he was from what planet?"
          "Mars"
          "Pluto"
          "Saturn"
          "Jupiter")
      
(question "http://www.youtube.com/watch?v=LGbNrAxb_a4"
          "How did this heavy metal guitarist die?"
          "Murder"
          "Overdose"
          "Car accident"
          "Suicide")
      
(question "http://www.youtube.com/watch?v=scdm5il-hC4"
          "What television show used this song in its intro?"
          "Roswell"
          "Fringe"
          "X Files"
          "Battlestar Galactica")

(mac mapair (x y f xs)
 `(map (fn ((,x ,y)) ,f)
       (pair ,xs)))

(def param args
  (mapair n v
          (tag (param name n value v))
          args))

(def embed (url)
  (tag (embed src url
              type "application/x-shockwave-flash"
              allowscriptaccess "always"
              allowfullscreen "true"
              width 560 height 340)))

(def video (url)
  (tag (object width 560 height 340)
    (param 'movie url
           'allowFullScreen 'true
           'allowscriptaccess 'always)
    (embed url)))

(def shuffle (xs)
  (accum a
    (let r (range 0 (- len.xs 1))
      (while r
        (let n (rand-elt r)
          (a xs.n)
          (pull n r))))))

(def choices (correct b c d)
  (let letters '("A" "B" "C" "D")
    (each answer (shuffle (list correct b c d))
      (aform (fn (req) (if (is answer correct)
                           (pr "Correct!")
                           (pr "The correct answer is: " correct))
                       (br2)
                       (form "q" (submit "Next Question")))
        (submit (pop letters))
        (sp)
        (pr answer)))))

(defop q req
  (let q (questions* (between 1 question-id*))
    (pr q!q)
    (br2)
    (video q!url)
    (br2)
    (choices q!a q!b q!c q!d)))


