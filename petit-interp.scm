#! /usr/bin/env -S gsi -:dar

;;; Fichier : petit-interp.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la premiere section.

;;; Notez que les commentaires ne contiennent pas d'accents car cela
;;; permettra une lecture egale dans tous les editeurs...

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction parse-and-execute
;;; doit etre definie, et vous pouvez modifier et ajouter des
;;; definitions de fonction afin de bien decomposer le traitement a
;;; faire en petites fonctions.  Il faut vous limiter au sous-ensemble
;;; *fonctionnel* de Scheme dans votre codage (donc n'utilisez pas
;;; set!, set-car!, vector-set!, list-set!, begin, print, display,
;;; etc).

(define-macro (one-of val =? lst)
  (cond ((null? lst) #f)
        ((null? (cdr lst))
         `(,=? ,val ,(car lst)))
        (else
         `(or (,=? ,val ,(car lst))
              (one-of ,val ,=? ,(cdr lst))))))

;; La fonction parse-and-execute recoit en parametre une liste des
;; caracteres qui constituent le programme a interpreter.  La
;; fonction retourne une chaine de caracteres qui sera imprimee comme
;; resultat final du programme.  S'il y a une erreur lors de
;; l'analyse syntaxique ou lors de l'execution, cette chaine de
;; caracteres contiendra un message d'erreur pertinent.  Sinon, la
;; chaine de caracteres sera l'accumulation des affichages effectues
;; par les enonces "print" executes par le programme interprete.

(define parse-and-execute
  (lambda (inp)
    (parse inp execute)))

;; La fonction next-sym recoit deux parametres, une liste de
;; caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole.  La continuation
;; sera appelee avec deux parametres, la liste des caracteres restants
;; (apres le symbole analyse) et le symbole qui a ete lu (soit un
;; symbole Scheme ou une chaine de caractere Scheme dans le cas d'un
;; <id> ou un entier Scheme dans le cas d'un <int>).  S'il y a une
;; erreur d'analyse (tel un caractere inapproprie dans la liste de
;; caracteres) la fonction next-sym retourne une chaine de caracteres
;; indiquant une erreur de syntaxe, sans appeler la continuation.

(define next-sym
  (lambda (inp cont)
    (cond ((null? inp)
           (cont inp 'EOI)) ;; retourner symbole EOI a la fin de l'input
          ((blanc? (@ inp))
           (next-sym ($ inp) cont)) ;; sauter les blancs
          (else
           (let ((c (@ inp)))
             (pp c)
             (pp #\newline)
             (cond ((chiffre? c)   (symbol-int inp cont))
                   ((lettre? c)    (symbol-id inp cont))
                   ((char=? c #\{)  (cont ($ inp) 'LBRK));; { Left Bracket
                   ((char=? c #\})  (cont ($ inp) 'RBRK));; } Right Bracket
                   ((char=? c #\()  (cont ($ inp) 'LPAR)) ;; (
                   ((char=? c #\))  (cont ($ inp) 'RPAR)) ;; )
                   ((char=? c #\;)  (cont ($ inp) 'SEMI)) ;; ;
                   ((char=? c #\+)  (cont ($ inp) 'ADD)) ;; +
                   ((char=? c #\-)  (cont ($ inp) 'SUB)) ;; -
                   ((char=? c #\*)  (cont ($ inp) 'MUL)) ;; *
                   ((char=? c #\/)  (cont ($ inp) 'DIV)) ;; /
                   ((char=? c #\%)  (cont ($ inp) 'MOD)) ;; %
                   (else
                    (let ((c2 (@ ($ inp))))
                      (pp 'c2)
                      (pp c2)
                      (cond ((char=? c #\=)
                             (if (char=? c2 #\=)
                                 (cont ($ ($ inp)) 'EQ)
                                 (cont ($ inp) 'ASSIGN))) ;; ==
                            ((char=? c #\>)
                             (if (char=? c2 #\=)
                                 (cont ($ ($ inp)) 'BTEQ)     ;; >=
                                 (cont ($ inp) 'BT)))     ;; >
                            ((char=? c #\<)
                             (if (char=? c2 #\=)
                                 (cont ($ ($ ($ inp))) 'LTEQ)     ;; <=
                                 (cont ($ inp) 'LT)))     ;; <
                            ((char=? c #\!)
                             (if (char=? c2 #\=)
                                 (cont ($ ($ inp)) 'NEQ)      ;; !=
                                 (syntax-err)))
                            (else
                             (syntax-err)))))))))))

;; La fonction @ prend une liste de caractere possiblement vide et
;; retourne le premier caractere, ou le caractere #\nul si la liste
;; est vide.

(define @
  (lambda (inp)
    (if (null? inp) #\nul (car inp))))

;; La fonction $ prend une liste de caractere possiblement vide et
;; retourne la liste des caracteres suivant le premier caractere s'il
;; y en a un.

(define $
  (lambda (inp)
    (if (null? inp) '() (cdr inp))))

;; La fonction syntax-err retourne le message d'erreur indiquant une
;; erreur de syntaxe.

(define syntax-err
  (lambda ()
    "syntax error\n"))

;; La fonction blanc? teste si son unique parametre est un caractere
;; blanc.

(define blanc?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab) (char=? c #\return))))

;; La fonction chiffre? teste si son unique parametre est un caractere
;; numerique.

(define chiffre?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

;; La fonction lettre? teste si son unique parametre est une lettre
;; minuscule.

(define lettre?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

;; La fonction symbol-int recoit deux parametres, une liste de
;; caracteres qui debute par un chiffre et une continuation.  La liste
;; de caracteres sera analysee pour en extraire le symbole <int>.  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole <int> analyse et le symbole
;; <int> qui a ete lu (un entier Scheme qui est la valeur numerique du
;; symbole <int>).

(define symbol-int
  (lambda (inp cont)
    (symbol-int-aux inp cont 0)))

(define symbol-int-aux
  (lambda (inp cont n)
    (if (chiffre? (@ inp))
        (symbol-int-aux ($ inp)
                        cont
                        (+ (* 10 n) (- (char->integer (@ inp)) 48)))
        (cont inp n))))

;; La fonction symbol-id recoit deux parametres, une liste de
;; caracteres qui debute par une lettre minuscule et une continuation.
;; La liste de caracteres sera analysee pour en extraire le prochain
;; symbole (soit un mot cle comme "print" ou un <id>).  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole analyse et le symbole qui a
;; ete lu (soit un symbole Scheme, comme PRINT-SYM, ou une chaine de
;; caracteres Scheme qui correspond au symbole <id>).

(define symbol-id
  (lambda (inp cont)
    (symbol-id-aux inp cont '())))

(define symbol-id-aux
  (lambda (inp cont lst)
    (if (lettre? (@ inp))
        (symbol-id-aux ($ inp) cont (cons (@ inp) lst))
        (let ((id (list->string (reverse lst))))
          (cond ((string=? id "print")
                 (cont inp 'PRINT-SYM))
                (else
                 (cont inp id)))))))

;; La fonction expect recoit trois parametres, un symbole, une liste
;; de caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole qui doit etre le meme
;; que le premier parametre de la fonction.  Dans ce cas la
;; continuation sera appelee avec un parametre, la liste des
;; caracteres restants apres le symbole analyse.  Si le prochain
;; symbole n'est pas celui qui est attendu, la fonction expect
;; retourne une chaine de caracteres indiquant une erreur de syntaxe.

(define expect
  (lambda (expected-sym inp cont)
    (next-sym inp
              (lambda (inp sym)
                (if (equal? sym expected-sym)
                    (cont inp)
                    (syntax-err))))))

;; La fonction parse recoit deux parametres, une liste de caracteres
;; et une continuation.  La liste de caracteres sera analysee pour
;; verifier qu'elle est conforme a la syntaxe du langage.  Si c'est le
;; cas, la continuation sera appelee avec une S-expression qui
;; represente l'ASA du programme.  Sinon la fonction parse retourne
;; une chaine de caracteres indiquant une erreur de syntaxe.

(define parse
  (lambda (inp cont)
    (<program> inp ;; analyser un <program>
               (lambda (inp program)
                 (expect 'EOI ;; verifier qu'il n'y a rien apres
                         inp
                         (lambda (inp)
                           (cont program)))))))

;; Les fonctions suivantes, <program>, <stat>, ... recoivent deux
;; parametres, une liste de caracteres et une continuation.  La liste
;; de caracteres sera analysee pour verifier qu'elle debute par une
;; partie qui est conforme a la categorie correspondante de la
;; grammaire du langage.  Si c'est le cas, la continuation sera
;; appelee avec deux parametres : une liste des caracteres restants du
;; programme et une S-expression qui represente l'ASA de ce fragment
;; de programme.  Sinon ces fonctions retournent une chaine de
;; caracteres indiquant une erreur de syntaxe.

(define <program>
  (lambda (inp cont)
    (<stat> inp cont))) ;; analyser un <stat>

(define <stat>
  (lambda (inp cont)
    (next-sym inp
              (lambda (inp2 sym)
                (case sym ;; determiner quel genre de <stat>
                  ((PRINT-SYM)
                   (<print_stat> inp2 cont))
                  ((LBRK)
                   (<bracket_stat> inp2 '() cont))
                  (else
                   (<expr_stat> inp cont)))))))
(trace <stat>)

(define <bracket_stat>
  (lambda (inp listeStat cont)
    (<stat> inp ;;fait le statement a l'interieur
            (lambda (inp2 sym);;regarde le symbole apres
              (next-sym inp2
                        (lambda(inp3 sym2)
                          (cond
                                ((equal? sym2 'EOI);;si on atteint la fin du document sans trouver un '}'
                                 (syntax-err))
                                ((equal? sym2 'RBRK);;si on trouve un '}'
                                 (cont inp3(append listeStat (list sym '(ES)))));; on ajoute un bloc statement ((<stat>)())
                                (else
                                 (<bracket_stat> inp2
                                                 (append listeStat (list sym))
                                                 cont)))))))))

(trace <bracket_stat>)

(define <print_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;; analyser un <paren_expr>
                  (lambda (inp expr)
                    (expect 'SEMI ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (cont inp
                                    (list 'PRINT expr))))))))
(trace <print_stat>)

;; "(" <expr> ")
(define <paren_expr>
  (lambda (inp cont)
    (expect 'LPAR ;; doit debuter par "("
            inp
            (lambda (inp)
              (<expr> inp ;; analyser un <expr>
                      (lambda (inp expr)
                        (expect 'RPAR ;; doit etre suivi de ")"
                                inp
                                (lambda (inp)
                                  (cont inp
                                        expr)))))))))
(trace <paren_expr>)

(define <expr_stat>
  (lambda (inp cont)
    (<expr> inp ;; analyser un <expr>
            (lambda (inp expr)
              (expect 'SEMI ;; doit etre suivi de ";"
                      inp
                      (lambda (inp)
                        (cont inp
                              (list 'EXPR expr))))))))
(trace <expr_stat>)

;;<test> | <id> "=" <expr>
(define <expr>
  (lambda (inp cont)
    (next-sym inp ;; verifier 1e symbole du <expr>
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <expr>
                          (lambda (inp3 sym2)
                            (if (and (string? sym1) ;; combinaison "id =" ?
                                     (equal? sym2 'ASSIGN))
                                (<expr> inp3
                                        (lambda (inp expr)
                                          (cont inp
                                                (list 'EQ
                                                      sym1
                                                      expr))))
                                (<test> inp cont))))))))
(trace <expr>)

;;<sum> | <sum> "<" <sum> | <sum> "<=" <sum> | <sum> "<=" <sum> | <sum> "<=" <sum>
;;| <sum> ">" <sum> | <sum> ">=" <sum> | <sum> "==" <sum> | <sum> "!=" <sum>
(define <test>
  (lambda (inp cont)
    (<sum> inp
           '()
           (lambda (inp2 sym1) ;; gets next sym
             (next-sym inp2
                       (lambda (inp3 sym2) ;; gets next sym
                         (if (one-of sym2 equal? (list 'LT 'BT 'LEQ 'BEQ 'EQ 'ASSIGN 'NEQ))
                             (<sum> inp3 ;; calls sum on next sym
                                    ;; the if will help format properly the operations
                                    '()
                                    (lambda (inp4 expr)
                                      (cont inp4
                                            (list sym2
                                                  sym1
                                                  expr))))
                          (<sum> inp '() cont))))))))
(trace <test>)
;;<mult> | <sum> "+" <mult> | <sum> "-" <mult>
(define <sum>
  (lambda (inp listeSum cont)
    (<mult> inp
            '()
            (lambda (inp2 sym1) ;; gets next sym
              (next-sym inp2
                        (lambda (inp3 sym2) ;; gets next sym
                          (if (one-of sym2 equal? (list 'ADD 'SUB))
                              (<sum> inp3 ;; calls sum on next sym
                                     ;; the if will help format properly the operations
                                     (if (null? listeSum) ;; premiere rec
                                         (cons sym2 (list sym1))
                                         (cons sym2
                                               (list (append listeSum
                                                             (list sym1)))))
                                     cont)
                              (cont inp2
                                    ;; term | (sym (term)) | (sym (sym .... ))
                                    (if (null? listeSum)
                                        sym1
                                        (append listeSum (list sym1)))))))))))

;;<term> | <mult> "*" <term> | <mult> "/" <term> | <mult> "%" <term>
(define <mult>
  (lambda (inp listeMult cont)
    (<term> inp
            (lambda (inp2 sym1)
              (next-sym inp2
                        (lambda (inp3 sym2)
                          (if (one-of sym2 equal? (list 'MOD 'MUL 'DIV))
                              (<mult> inp3
                                      (if (null? listeMult)
                                          (cons sym2 (list sym1))
                                          (cons sym2
                                                (list (append listeMult
                                                              (list sym1)))))
                                      cont)
                              (cont inp2
                                    ;; term | (sym (term))| (sym (sym .... ))
                                    (if (null? listeMult)
                                           sym1
                                           (append listeMult
                                                   (list sym1)))))))))))
(trace <mult>)

;;<id> | <int>| <parent_expr>
(define <term>
  (lambda (inp cont)
    (next-sym inp ;; verifier le premier symbole du <term>
              (lambda (inp2 sym)
                (cond ((string? sym) ;; identificateur?
                       (cont inp2 (list 'VAR sym)))
                      ((number? sym) ;; entier?
                       (cont inp2 (list 'INT sym)))
                      (else
                       (<paren_expr> inp cont)))))))
(trace <term>)
;; La fonction execute prend en parametre l'ASA du programme a
;; interpreter et retourne une chaine de caracteres qui contient
;; l'accumulation de tout ce qui est affiche par les enonces "print"
;; executes par le programme interprete.

(define execute
  (lambda (ast)
    (exec-stat '() ;; etat des variables globales
               ""  ;; sortie jusqu'a date
               ast ;; ASA du programme
               (lambda (env output)
                 output)))) ;; retourner l'output pour qu'il soit affiche

;; La fonction exec-stat fait l'interpretation d'un enonce du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'enonce a interpreter et une continuation.  La continuation sera
;; appelee avec deux parametres : une liste d'association donnant la
;; valeur de chaque variable du programme apres l'interpretation de
;; l'enonce et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'enonce.

(define exec-stat
  (lambda (env output ast cont)
    (display "ast courrant: ")(display ast)(newline)
    (display "stat courrant: ")(display (car ast))(newline)
    (case (car(car ast))

      ((PRINT)
       (exec-expr env ;; evaluer l'expression du print
                  output
                  (cadr(car ast))
                  (lambda (env output val)
                    (exec-stat env ;; ajouter le resultat a la sortie
                               (cond ((number? val)
                                      (string-append output
                                                     (number->string val)
                                                     "\n"))
                                     (else
                                      (string-append output
                                                     (if val
                                                         "true"
                                                         "false")
                                                     "\n")))
                               (cdr ast)
                               cont))))

      ((EXPR)
       (exec-expr env ;; evaluer l'expression
                  output
                  (cadr (car ast))
                  (lambda (env output val)
                    (cont env output)))) ;; continuer en ignorant le resultat

      ((ES);;END of Statement TODO peut probablement juste faire un () vide, mais faudra le traiter plus haut
       (cont env output))

      (else
       "internal error (unknown statement AST)\n"))))

;; La fonction exec-expr fait l'interpretation d'une expression du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'expression a interpreter et une continuation.  La continuation
;; sera appelee avec deux parametres : une liste d'association donnant
;; la valeur de chaque variable du programme apres l'interpretation de
;; l'expression et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'expression.

(define exec-expr
  (lambda (env output ast cont)
    (define (exec-op op)
      (let ((func (lambda (elem) (exec-expr env
                            output
                            elem;;(car (cdr ast))
                            (lambda(env output val)
                              val)))))
        (let ((num1 (func (cadr ast)))
              (num2 (func (caddr ast))))
          (cont env
                output
                (op num1 num2)))))
    (case (car ast)
      ((INT)
       (cont env
             output
             (cadr ast))) ;; retourner la valeur de la constante
      ((ADD)
       (exec-op + ))
      ((SUB)
       (exec-op -))
      ((MUL)
       (exec-op *))
      ((DIV)
       (exec-op /))
      ((MOD)
       (exec-op modulo))
      ((LT)
       (exec-op <))
      ((BT)
       (exec-op >))
    (else
     "internal error (unknown expression AST)\n"))))

;; Il faut enlever la trace avant la remise...

(trace main parse-and-execute execute <program> <expr>)

;;;----------------------------------------------------------------------------

;;; *** NE MODIFIEZ PAS CETTE SECTION ***

(define main
  (lambda ()
    (print (parse-and-execute (read-all (current-input-port) read-char)))))

;;;----------------------------------------------------------------------------
