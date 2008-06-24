;;; -*- mode: lisp; syntax: common-lisp; package: CL-GRANNY; encoding: utf-8 -*-
;;; $Id: gambol.asd,v 1.1 2008-06-15 16:49:46-05 annis Exp annis $
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;; This is free software; you can redistribute it and/or modify it
;;; under the same terms as Perl (the Artistic Licence).  Developed at
;;; the Department of Biostatistics and Medical Informatics, University
;;; of Wisconsin, Madison. 

(in-package :asdf)

(defsystem :gambol
  :name "GAMBOL"
  :author "William S. Annis <annis@biostat.wisc.edu>"
  :version "0.01"
  :maintainer "William S. Annis <annis@biostat.wisc.edu>"
  :licence "MIT License"
  :description "A small prolog library based on Frolic"

  :components ((:file "package")
               (:file "prolog"))
  :serial t)

