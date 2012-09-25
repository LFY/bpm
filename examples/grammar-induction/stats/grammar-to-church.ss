(import (printing)
        (util)
        (_srfi :1))

(define (rewrite-choose expr)
  (let* ([choices (cdr expr)]
         [delayed-choices (map (lambda (c) `(lambda () ,c))
                               choices)])
    `(discrete-sample ,@delayed-choices)))

(define (rewrite-body body)
  (if (equal? 'choose (car body))
    (rewrite-choose body)
    body))

(define (rewrite-nonterminal nt)
  (let* ([name (cadr nt)]
         [expr (cadddr nt)])
    `(define ,name
       (lambda ()
         ,(rewrite-body expr)))))

(define (rewrite-grammar gr)
  (let*
    ([nonterminals (cadr gr)]
     [start (caddr gr)]
     [start-body (rewrite-body (caddr start))]
     [rewritten-nonterminals (map rewrite-nonterminal nonterminals)])
    `(

      (define (contact . xs) (cons 'contact xs))
      (define (footer . xs) (cons 'footer xs))
      (define (text_link . xs) (cons 'text_link xs))
      (define (fine_print . xs) (cons 'fine_print xs))
      (define (text . xs) (cons 'text xs))
      (define (image . xs) (cons 'image xs))
      (define (heading . xs) (cons 'heading xs))
      (define (container . xs) (cons 'container xs))
      (define (threeColumn . xs) (cons 'threeColumn xs))
      (define (headline . xs) (cons 'headline xs))
      (define (hero_image . xs) (cons 'hero_image xs))
      (define (navigation_element . xs) (cons 'navigation_element xs))
      (define (logo . xs) (cons 'logo xs))
      (define (navigation . xs) (cons 'navigation xs))
      (define (page_root . xs) (cons 'page_root xs))
      (define (copyright . xs) (cons 'copyright xs))
      (define (twoColumn . xs) (cons 'twoColumn xs))
      (define (header . xs) (cons 'header xs))
      (define (auxiliary_navigation . xs) (cons 'auxiliary_navigation xs))
      (define (social_media . xs) (cons 'social_media xs))
      (define (subheading . xs) (cons 'subheading xs))
      (define (search . xs) (cons 'search xs))
      (define (login . xs) (cons 'login xs))
      (define (region . xs) (cons 'region xs))
      (define (image_link . xs) (cons 'image_link xs))
      (define (fourColumn . xs) (cons 'fourColumn xs))
      (define (form . xs) (cons 'form xs))
      (define (shopping_links . xs) (cons 'shopping_links xs))
      (define (sitemap . xs) (cons 'sitemap xs))
      (define (transaction_icons . xs) (cons 'transaction_icons xs))
      (define (auxiliary_navigtion . xs) (cons 'auxiliary_navigtion xs))
      (define (pagination . xs) (cons 'pagination xs))

      (define (uniform-select xs)
        (let* ([idx (sample-integer (length xs))])
          (list-ref xs idx)))

      (define (discrete-sample . choices)
        (let* ([which-choice (uniform-select choices)])
          (if (procedure? which-choice)
            (which-choice)
            which-choice)))
      ,@rewritten-nonterminals
      (define sample-grammar (lambda () ,start-body)))))

(define gr-filename (cadr (command-line)))

(load gr-filename)

(for-each pretty-print (rewrite-grammar grammar))
