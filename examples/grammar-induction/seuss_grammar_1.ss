(import (printing) (_srfi :1) (scene-graphs))
(define grammar
  '(program
     ((abstraction F157 ()
        (choose
          (elem "elt17" (tr "trans80" (F150))
            (tr "trans79" (F41)) (tr "trans78" (F42))
            (tr "trans77" (F122)))
          (elem "elt17" (tr "trans70" (F154))
            (tr "trans69" (F122)) (tr "trans66" (F155))
            (tr "trans51" (F156)))
          (elem "elt17" (tr "trans112" (F150))
            (tr "trans111" (F2)) (tr "trans110" (F3)))
          (elem "elt17" (tr "trans34" (F0))
            (tr "trans33" (F2)) (tr "trans32" (F17))
            (tr "trans31" (F94)) (tr "trans30" (F95))
            (tr "trans29" (F123)) (tr "trans28" (F95))
            (tr "trans27" (F98)))
          (elem "elt17" (tr "trans58" (F155))
            (tr "trans55" (F155)) (tr "trans54" (F3))
            (tr "trans51" (F156)) (tr "trans47" (F155)))
          (elem "elt17" (tr "trans87" (F0))
            (tr "trans29" (F123)) (tr "trans86" (F95))
            (tr "trans31" (F94)) (tr "trans85" (F74)))
          (elem "elt17" (tr "trans102" (F0))
            (tr "trans101" (F3)))))
       (abstraction F156 ()
         (choose (elem "elt10" (tr "trans95" (F0)))
           (elem "elt10" (tr "trans107" (F6))
             (tr "trans106" (F7)) (tr "trans105" (F8)))
           (elem "elt10" (tr "trans26" (F125))
             (tr "trans25" (F126)))
           (elem "elt10" (tr "trans92" (F125))
             (tr "trans91" (F126)))
           (elem "elt10" (tr "trans53" (F6))
             (tr "trans52" (F126)))
           (elem "elt10" (tr "trans12" (F124))
             (tr "trans11" (F126)) (tr "trans10" (F98)))
           (elem "elt10" (tr "trans53" (F6))
             (tr "trans65" (F42)) (tr "trans64" (F63)))))
       (abstraction F155 ()
         (choose
           (elem "elt14" (tr "trans50" (F73))
             (tr "trans49" (F74)) (tr "trans48" (F2)))
           (elem "elt14" (tr "trans68" (F126))
             (tr "trans67" (F73)))
           (elem "elt14" (tr "trans63" (F157))
             (tr "trans62" (F122)))
           (elem "elt14" (tr "trans19" (F154))
             (tr "trans18" (F120)))
           (elem "elt14" (tr "trans57" (F2))
             (tr "trans56" (F2)))
           (elem "elt14" (tr "trans46" (F126))
             (tr "trans45" (F120)) (tr "trans44" (F125)))
           (elem "elt14" (tr "trans60" (F63))
             (tr "trans59" (F0)))))
       (abstraction F154 ()
         (choose (elem "elt9")
           (elem "elt9" (tr "trans9" (F156)))
           (elem "elt9" (tr "trans84" (F157)))))
       (abstraction F153 ()
         (choose
           (elem "elt12" (tr "trans17" (F155))
             (tr "trans16" (F63)) (tr "trans15" (F122))
             (tr "trans14" (F127)))
           (elem "elt12" (tr "trans100" (F157))
             (tr "trans99" (F154)) (tr "trans98" (F0))
             (tr "trans97" (F17)) (tr "trans96" (F18))
             (tr "trans94" (F156)) (tr "trans93" (F7)))
           (elem "elt12" (tr "trans43" (F155))
             (tr "trans42" (F122)) (tr "trans41" (F122))
             (tr "trans36" (F150)) (tr "trans35" (F127)))
           (elem "elt12" (tr "trans24" (F156))
             (tr "trans23" (F122)) (tr "trans22" (F120))
             (tr "trans21" (F105)) (tr "trans20" (F17))
             (tr "trans13" (F153)))
           (elem "elt12" (tr "trans24" (F156))
             (tr "trans90" (F120)) (tr "trans89" (F122))
             (tr "trans88" (F122)) (tr "trans83" (F154))
             (tr "trans82" (F98)))
           (elem "elt12" (tr "trans76" (F157))
             (tr "trans75" (F63)) (tr "trans74" (F122))
             (tr "trans73" (F127)) (tr "trans72" (F126))
             (tr "trans71" (F49)) (tr "trans61" (F155)))))
       (abstraction F150 ()
         (choose
           (elem "elt7" (tr "trans39" (F85))
             (tr "trans81" (F122)))
           (elem "elt7" (tr "trans113" (F0)))
           (elem "elt7" (tr "trans40" (F122))
             (tr "trans39" (F85)) (tr "trans38" (F86))
             (tr "trans37" (F18)))
           (elem "elt7" (tr "trans7" (F120)))))
       (abstraction F134 ()
         (choose
           (elem "elt0" (tr "trans109" (F157))
             (tr "trans108" (F2)) (tr "trans104" (F156))
             (tr "trans103" (F3)))
           (elem "elt0" (tr "trans8" (F154))
             (tr "trans6" (F150)) (tr "trans5" (F122))
             (tr "trans4" (F123)) (tr "trans3" (F124))
             (tr "trans2" (F125)) (tr "trans1" (F126))
             (tr "trans0" (F127)))))
       (abstraction F120 () (elem "elt8"))
       (abstraction F122 () (elem "elt6"))
       (abstraction F123 () (elem "elt5"))
       (abstraction F124 () (elem "elt4"))
       (abstraction F125 () (elem "elt3"))
       (abstraction F0 () (elem "elt21"))
       (abstraction F126 () (elem "elt2"))
       (abstraction F127 () (elem "elt1"))
       (abstraction F2 () (elem "elt20"))
       (abstraction F3 () (elem "elt28"))
       (abstraction F63 () (elem "elt13"))
       (abstraction F6 () (elem "elt27"))
       (abstraction F7 () (elem "elt32"))
       (abstraction F8 () (elem "elt33"))
       (abstraction F73 () (elem "elt26"))
       (abstraction F74 () (elem "elt25"))
       (abstraction F17 () (elem "elt15"))
       (abstraction F85 () (elem "elt24"))
       (abstraction F18 () (elem "elt22"))
       (abstraction F86 () (elem "elt23"))
       (abstraction F94 () (elem "elt19"))
       (abstraction F95 () (elem "elt18"))
       (abstraction F98 () (elem "elt11"))
       (abstraction F105 () (elem "elt16"))
       (abstraction F41 () (elem "elt31"))
       (abstraction F42 () (elem "elt29"))
       (abstraction F49 () (elem "elt30")))
     (lambda ()
       (choose (F134) (F153) (F153) (F153) (F157) (F153)
         (F157) (F153) (F134)))))
(define elements
  '((elt30
      (dae:instance_geometry (\x40; (url "#stairs_S3Shape"))
        (dae:bind_material
          (dae:technique_common
            (dae:instance_material
              (\x40; (target "#lambert1_1")
                (symbol "lambert1_1SG")))))))
     (elt31
       (dae:instance_geometry
         (\x40; (url "#balcony_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt1
       (dae:instance_geometry
         (\x40; (url "#window_1_SShape4"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt18
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape17_1"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt0
       (dae:instance_geometry (\x40; (url "#base_2_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt19
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape23"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt23
       (dae:instance_geometry
         (\x40; (url "#door_2_SShape12"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt3
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape10"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt22
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape44"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt2
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape89"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt21
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape43"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))))))
     (elt20
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape159"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))))))
     (elt14
       (dae:instance_geometry
         (\x40; (url "#facade_1_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt5
       (dae:instance_geometry (\x40; (url "#door_1_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt15
       (dae:instance_geometry
         (\x40; (url "#door_2_SShape9"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt4
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape17"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt9
       (dae:instance_geometry
         (\x40; (url "#planarTrimmedSurfaceShape7"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt27
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape16"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt16
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape9"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt7
       (dae:instance_geometry
         (\x40; (url "#facade_3_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt8
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape133"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))))))
     (elt26
       (dae:instance_geometry (\x40; (url "#stairs_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt6
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape129"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt17
       (dae:instance_geometry
         (\x40; (url "#facade_2_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt25
       (dae:instance_geometry
         (\x40; (url "#door_2_SShape10"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt24
       (dae:instance_geometry (\x40; (url "#tower_S1Shape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt29
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape80"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt28
       (dae:instance_geometry
         (\x40; (url "#polySurfaceShape105"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert5")
                 (symbol "lambert5SG")))))))
     (elt10
       (dae:instance_geometry (\x40; (url "#base_1_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))))))
     (elt11
       (dae:instance_geometry (\x40; (url "#tower_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt12
       (dae:instance_geometry (\x40; (url "#base_3_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#stucco") (symbol "stuccoSG")))
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt32
       (dae:instance_geometry
         (\x40; (url "#door_2_SShape11"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))
             (dae:instance_material
               (\x40; (target "#lambert6")
                 (symbol "lambert6SG")))
             (dae:instance_material
               (\x40; (target "#lambert4")
                 (symbol "lambert4SG")))))))
     (elt13
       (dae:instance_geometry
         (\x40; (url "#planarTrimmedSurfaceShape5"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))
     (elt33
       (dae:instance_geometry
         (\x40; (url "#half_roof_SShape"))
         (dae:bind_material
           (dae:technique_common
             (dae:instance_material
               (\x40; (target "#lambert1_1")
                 (symbol "lambert1_1SG")))))))))
(define transforms
  '((trans70
      (dae:matrix (\x40; (sid "transform"))
        "-1 0 0 -10.5148 0 1 0 5.979502 0 0 -1 0.095436 0 0 0 1"))
     (trans65
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.219222 0 1 0 -3.484865 0 0 1 2.613536 0 0 0 1"))
     (trans14
       (dae:matrix (\x40; (sid "transform"))
         "0.9685495 0.2488208 0 -0.774971 -0.2488208 0.9685495 0 -3.032996 0 0 1 8.621319 0 0 0 1"))
     (trans71
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.289891 0 1 0 -13.58642 0 0 1 9.608893 0 0 0 1"))
     (trans64
       (dae:matrix (\x40; (sid "transform"))
         "0 0.08169613 -0.9966573 2.175351 0 -0.9966573 -0.08169613 7.606823 -1 0 0 -0.981387 0 0 0 1"))
     (trans15
       (dae:matrix (\x40; (sid "transform"))
         "0.9685495 0.2488208 0 -6.057772 -0.2488208 0.9685495 0 9.922496 0 0 1 4.652923 0 0 0 1"))
     (trans16
       (dae:matrix (\x40; (sid "transform"))
         "0 -0.7805664 0.6250729 10.5596 0 0.6250729 0.7805664 8.467156 -1 0 0 6.344552 0 0 0 1"))
     (trans94
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -7.8632 0 1 0 -11.00987 0 0 1 9.156451 0 0 0 1"))
     (trans17
       (dae:matrix (\x40; (sid "transform"))
         "0.9685495 0.2488208 0 1.9627 -0.2488208 0.9685495 0 19.1682 0 0 1 0.183164 0 0 0 1"))
     (trans95
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.131139 0 1 0 -2.918497 0 0 1 3.608069 0 0 0 1"))
     (trans76
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.170119 0 1 0 21.53002 0 0 1 -2.559772 0 0 0 1"))
     (trans49
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 0.409472 0 1 0 -6.029636 0 0 1 4.3609 0 0 0 1"))
     (trans63
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -11.18654 0 1 0 5.114365 0 0 1 -2.797407 0 0 0 1"))
     (trans96
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -3.358541 0 1 0 -3.686699 0 0 1 9.329312 0 0 0 1"))
     (trans77
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -6.859089 0 1 0 3.454097 0 0 1 1.78287 0 0 0 1"))
     (trans48
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -1.532311 0 1 0 4.125332 0 0 1 4.278367 0 0 0 1"))
     (trans62
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.437455 0 1 0 -1.253927 0 0 1 1.425164 0 0 0 1"))
     (trans81
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.531053 0 1 0 10.8042 0 0 1 1.217286 0 0 0 1"))
     (trans97
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -3.416707 0 1 0 -2.881361 0 0 1 8.055081 0 0 0 1"))
     (trans74
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -8.233084 0 1 0 11.2869 0 0 1 4.536476 0 0 0 1"))
     (trans61
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -10.03865 0 1 0 -5.31894 0 0 1 0 0 0 0 1"))
     (trans80
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 4.815835 0 1 0 16.56403 0 0 1 -0.948966 0 0 0 1"))
     (trans10
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 -0.469107 0 1 0 7.853967 -1 0 0 0.750464 0 0 0 1"))
     (trans75
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 -3.853522 0 1 0 14.64069 -1 0 0 6.11861 0 0 0 1"))
     (trans60
       (dae:matrix (\x40; (sid "transform"))
         "0 0.3199634 -0.9474299 -3.982464 0 -0.9474299 -0.3199634 12.73489 -1 0 0 3.381549 0 0 0 1"))
     (trans109
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 6.630658 0 1 0 13.87609 0 0 1 -0.360232 0 0 0 1"))
     (trans83
       (dae:matrix (\x40; (sid "transform"))
         "-0.195298 0.9807439 0 12.32158 -0.9807439 -0.195298 0 -4.770616 0 0 1 3.839748 0 0 0 1"))
     (trans11
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 0.19165 0 1 0 -0.754317 0 0 1 3.15149 0 0 0 1"))
     (trans108
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 5.026134 0 1 0 1.572405 0 0 1 6.087463 0 0 0 1"))
     (trans82
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 0.104718 0 1 0 16.71485 -1 0 0 7.077807 0 0 0 1"))
     (trans8
       (dae:matrix (\x40; (sid "transform"))
         "0.2232877 -0.9747526 0 16.9578 0.9747526 0.2232877 0 0.665951 0 0 1 0 0 0 0 1"))
     (trans12
       (dae:matrix (\x40; (sid "transform"))
         "0.760509 0 0 -0.306132 0 0.760509 0 -3.979951 0 0 0.760509 4.496932 0 0 0 1"))
     (trans38
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -2.604152 0 1 0 4.634833 0 0 1 3.442049 0 0 0 1"))
     (trans90
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -6.649935 0 1 0 -5.453539 0 0 1 7.110368 0 0 0 1"))
     (trans9
       (dae:matrix (\x40; (sid "transform"))
         "0.2232877 0.9747526 0 -3.874244 -0.9747526 0.2232877 0 -9.497069 0 0 1 1.737527 0 0 0 1"))
     (trans13
       (dae:matrix (\x40; (sid "transform"))
         "0.9685495 -0.2488208 0 -6.021332 0.2488208 0.9685495 0 25.46554 0 0 1 -0.940495 0 0 0 1"))
     (trans39
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 6.464609 0 -1 0 13.45997 1 0 0 3.237097 0 0 0 1"))
     (trans91
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.003741 0 1 0 -0.754317 0 0 1 2.820055 0 0 0 1"))
     (trans92
       (dae:matrix (\x40; (sid "transform"))
         "0.675018 0 0 -0.303282 0 0.675018 0 -4.154222 0 0 0.675018 5.166377 0 0 0 1"))
     (trans112
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -7.573817 0 1 0 -1.022873 0 0 1 -1.455686 0 0 0 1"))
     (trans85
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -2.17758 0 1 0 -7.013315 0 0 1 5.165346 0 0 0 1"))
     (trans93
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 0.952402 0 1 0 -12.28285 0 0 1 8.288698 0 0 0 1"))
     (trans113
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.09673 0 1 0 1.715294 0 0 1 4.754576 0 0 0 1"))
     (trans84
       (dae:matrix (\x40; (sid "transform"))
         "-0.195298 -0.9807439 0 -6.764377 0.9807439 -0.195298 0 10.58936 0 0 1 -2.285058 0 0 0 1"))
     (trans36
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -2.437945 0 1 0 12.81692 0 0 1 -4.290987 0 0 0 1"))
     (trans110
       (dae:matrix (\x40; (sid "transform"))
         "1.279056 0 0 -1.503881 0 1.279056 0 -3.026142 0 0 1.279056 5.118455 0 0 0 1"))
     (trans87
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.196096 0 1 0 4.736031 0 0 1 5.462666 0 0 0 1"))
     (trans37
       (dae:matrix (\x40; (sid "transform"))
         "0.739732 0 0 -2.654805 0 0.739732 0 1.652418 0 0 0.739732 4.572401 0 0 0 1"))
     (trans111
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.75922 0 1 0 6.858092 0 0 1 4.633001 0 0 0 1"))
     (trans86
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -9.214349 0 1 0 -11.28007 0 0 1 6.953157 0 0 0 1"))
     (trans34
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.254544 0 1 0 5.28301 0 0 1 5.412418 0 0 0 1"))
     (trans23
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -8.725797 0 1 0 3.799541 0 0 1 4.489516 0 0 0 1"))
     (trans35
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.3494211 0 1 0 -10.15948 0 0 1 8.747703 0 0 0 1"))
     (trans22
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.148748 0 1 0 4.053617 0 0 1 7.217401 0 0 0 1"))
     (trans21
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 3.628363 0 1 0 -2.651469 0 0 1 9.174433 0 0 0 1"))
     (trans103
       (dae:matrix (\x40; (sid "transform"))
         "1.279056 0 0 -5.919721 0 1.279056 0 -6.638026 0 0 1.279056 6.66363 0 0 0 1"))
     (trans89
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -1.124602 0 1 0 6.763353 0 0 1 4.423198 0 0 0 1"))
     (trans20
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 3.522587 0 1 0 -2.050209 0 0 1 8.120461 0 0 0 1"))
     (trans102
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.91459 0 1 0 5.810341 0 0 1 5.541421 0 0 0 1"))
     (trans88
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.69125 0 1 0 11.21777 0 0 1 4.423198 0 0 0 1"))
     (trans32
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -2.068856 0 1 0 -7.098414 0 0 1 4.228245 0 0 0 1"))
     (trans18
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.506716 0 1 0 0.452629 0 0 1 4.087012 0 0 0 1"))
     (trans2
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 5.443149 0 1 0 -8.951682 0 0 1 7.925804 0 0 0 1"))
     (trans101
       (dae:matrix (\x40; (sid "transform"))
         "1.279056 0 0 -1.187432 0 1.279056 0 -5.50385 0 0 1.279056 5.231855 0 0 0 1"))
     (trans19
       (dae:matrix (\x40; (sid "transform"))
         "-0.9928302 0.1195333 0 -8.327452 0.1195333 0.9928302 0 -5.803103 0 0 -1 2.689072 0 0 0 1"))
     (trans3
       (dae:matrix (\x40; (sid "transform"))
         "0.760509 0 0 -4.496492 0 0.760509 0 -9.211008 0 0 0.760509 5.931293 0 0 0 1"))
     (trans33
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -5.548299 0 1 0 -1.402388 0 0 1 4.674914 0 0 0 1"))
     (trans100
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 0.572729 0 1 0 25.15823 0 0 1 0.604616 0 0 0 1"))
     (trans30
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -8.961877 0 1 0 -11.28007 0 0 1 8.507847 0 0 0 1"))
     (trans0
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 6.073173 0 1 0 2.739913 0 0 1 5.98996 0 0 0 1"))
     (trans27
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 2.44813 0 1 0 18.83085 -1 0 0 2.018146 0 0 0 1"))
     (trans31
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -2.162856 0 1 0 -7.894305 0 0 1 6.11861 0 0 0 1"))
     (trans1
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 5.582785 0 1 0 -5.248412 0 0 1 6.556413 0 0 0 1"))
     (trans26
       (dae:matrix (\x40; (sid "transform"))
         "0.675018 0 0 -0.303282 0 0.675018 0 -4.154222 0 0 0.675018 4.653632 0 0 0 1"))
     (trans25
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.201219 0 1 0 -0.724577 0 0 1 2.803966 0 0 0 1"))
     (trans107
       (dae:matrix (\x40; (sid "transform"))
         "0.610909 0 0 0.169037 0 0.610909 0 -4.323035 0 0 0.610909 4.483428 0 0 0 1"))
     (trans24
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.323835 0 1 0 -10.66762 0 0 1 6.751116 0 0 0 1"))
     (trans106
       (dae:matrix (\x40; (sid "transform"))
         "0.787086 0 0 -0.005091 0 0.787086 0 -0.946922 0 0 0.787086 2.980128 0 0 0 1"))
     (trans6
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -7.616696 0 1 0 4.209119 0 0 1 0 0 0 0 1"))
     (trans45
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -1.412009 0 1 0 0.7129073 0 0 1 4.185456 0 0 0 1"))
     (trans50
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 0.470898 0 1 0 -7.821949 -1 0 0 6.751387 0 0 0 1"))
     (trans105
       (dae:matrix (\x40; (sid "transform"))
         "-0.787086 0 0 -0.048823 0 0.787086 0 3.070725 0 0 -0.787086 4.541757 0 0 0 1"))
     (trans7
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -3.519621 0 1 0 0.797705 0 0 1 5.522169 0 0 0 1"))
     (trans44
       (dae:matrix (\x40; (sid "transform"))
         "0.6750173 0 0 0.2550065 0 0.6750173 0 -9.502906 0 0 0.6750173 6.026897 0 0 0 1"))
     (trans51
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -4.366524 0 1 0 -10.46305 0 0 1 6.403013 0 0 0 1"))
     (trans104
       (dae:matrix (\x40; (sid "transform"))
         "1.270508 0 0 13.49685 0 1.270508 0 -3.350643 0 0 1.270508 4.902893 0 0 0 1"))
     (trans78
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 2.011939 0 1 0 4.838214 0 0 1 4.283164 0 0 0 1"))
     (trans4
       (dae:matrix (\x40; (sid "transform"))
         "0.616492 0 0 -4.498908 0 0.616492 0 -6.51174 0 0 0.616492 5.508788 0 0 0 1"))
     (trans47
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 9.540829 0 1 0 -5.114365 0 0 1 2.040076 0 0 0 1"))
     (trans52
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.233832 0 1 0 -0.504591 0 0 1 3.385789 0 0 0 1"))
     (trans79
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 2.01769 0 -1 0 5.379832 1 0 0 5.825748 0 0 0 1"))
     (trans5
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -9.922268 0 1 0 3.092108 0 0 1 2.834999 0 0 0 1"))
     (trans46
       (dae:matrix (\x40; (sid "transform"))
         "0.9999995 0 0 0.4921962 0 0.9999995 0 -6.05534 0 0 0.9999995 4.736227 0 0 0 1"))
     (trans53
       (dae:matrix (\x40; (sid "transform"))
         "0.776165 0 0 -0.098859 0 0.776165 0 -3.959792 0 0 0.776165 5.032473 0 0 0 1"))
     (trans29
       (dae:matrix (\x40; (sid "transform"))
         "0.626358 0 0 -1.444358 0 0.626358 0 -13.08426 0 0 0.626358 4.204014 0 0 0 1"))
     (trans28
       (dae:matrix (\x40; (sid "transform"))
         "-0.7836447 0 0.6212093 4.430555 0 1 0 -11.28007 -0.6212093 0 -0.7836447 9.680097 0 0 0 1"))
     (trans41
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -6.384014 0 1 0 12.48315 0 0 1 4.708888 0 0 0 1"))
     (trans54
       (dae:matrix (\x40; (sid "transform"))
         "1.279056 0 0 1.167688 0 1.279056 0 3.864678 0 0 1.279056 5.056351 0 0 0 1"))
     (trans40
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 1.929333 0 1 0 10.72599 0 0 1 1.119104 0 0 0 1"))
     (trans55
       (dae:matrix (\x40; (sid "transform"))
         "0.9646842 0.2634092 0 5.093634 -0.2634092 0.9646842 0 21.07756 0 0 1 -3.178971 0 0 0 1"))
     (trans69
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -1.400093 0 1 0 12.65113 0 0 1 4.050584 0 0 0 1"))
     (trans43
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -10.03865 0 1 0 -5.31894 0 0 1 7.316207 0 0 0 1"))
     (trans56
       (dae:matrix (\x40; (sid "transform"))
         "0.9646842 -0.2634092 0 1.331495 0.2634092 0.9646842 0 3.928971 0 0 1 4.471016 0 0 0 1"))
     (trans68
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.24236 0 1 0 -3.645783 0 0 1 4.349568 0 0 0 1"))
     (trans42
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 1.186784 0 1 0 5.646124 0 0 1 4.708888 0 0 0 1"))
     (trans57
       (dae:matrix (\x40; (sid "transform"))
         "0.9646842 -0.2634092 0 -0.930802 0.2634092 0.9646842 0 -2.011537 0 0 1 4.453139 0 0 0 1"))
     (trans98
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 1.350196 0 1 0 3.802939 0 0 1 7.89378 0 0 0 1"))
     (trans99
       (dae:matrix (\x40; (sid "transform"))
         "0.2232877 -0.9747526 0 -4.887365 0.9747526 0.2232877 0 15.41193 0 0 1 6.227126 0 0 0 1"))
     (trans72
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.104477 0 1 0 -8.999149 0 0 1 7.142558 0 0 0 1"))
     (trans67
       (dae:matrix (\x40; (sid "transform"))
         "0 0 1 -0.329765 0 1 0 -8.244452 -1 0 0 6.586534 0 0 0 1"))
     (trans58
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -9.991689 0 1 0 -5.114365 0 0 1 -1.580798 0 0 0 1"))
     (trans73
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -3.636039 0 1 0 -0.548845 0 0 1 8.09915 0 0 0 1"))
     (trans66
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -11.01886 0 1 0 -5.114365 0 0 1 2.040076 0 0 0 1"))
     (trans59
       (dae:matrix (\x40; (sid "transform"))
         "1 0 0 -0.749496 0 1 0 0.346899 0 0 1 5.000672 0 0 0 1"))))
(sample-multiple 20 "seuss_final_01.dae.scene"
  "seuss_final_01.dae" grammar elements transforms)
