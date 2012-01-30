;;
;;  Copyright (C) 30-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :cl-acpi-classes
  (:use :common-lisp :defclass-alt)
  (:nicknames :acpi)
  (:export acpi +classes-for+
	   charge-state charge-capacity on-line thermal-state thermal-trip cooling
	   kind nr
	   state fraction
	   design-capacity last-full-capacity
	   temp-unit
	   temp to-mode
	   of cooling-cnt total-cnt)
  (:documentation "Classes for :cl-acpi-command, was looking for a way to properly\
document, classes can do it."))

(in-package :cl-acpi-classes)

(defclass* acpi ()
  "Empty acpi entry"
  (kind "Kind of device it is about." keyword)
  (nr   "Number of device." fixnum))

(defconstant +classes-for+
  '((:battery charge-state charge-capacity)
    (:adapter on-line)
    (:thermal thermal-state thermal-trip)
    (:cooling cooling))
  "Assoc list of which classes belong to which kinds.")

(defclass* charge-state (acpi)
  "How much charge currently."
  (state  "Current state: :full, :charging or :discharging." keyword)
  (fraction "Fraction filled."))

(defclass* charge-capacity (acpi)
  "Design capacity and last top capacity attained."
  (design-capacity "Design capacity" number)
  (last-full-capacity "Last ful capacity" number))

(defclass* on-line (acpi) "Specifies if device is online.")

(defclass* temp-unit ()
  "Unit of any temperature that might be there."
  (temp-unit "Unit of temperature Celcius :c, or fahrenheit :f" keyword))

(defclass* thermal-state (acpi temp-unit)
 "Current temperature."
 (state "What the state is." keyword)
 (temp "Temperature." ))

(defclass* thermal-trip (acpi temp-unit)
 "Where the trip point of the temperature lies."
 (nr "Number of trip point" fixnum)
 (to-mode "Number of the mode it charges to." fixnum)
 (temp "Temperature at which this happens." number))

(defclass* cooling (acpi)
  "How many of the total of things are cooling."
  (of "The thing being cooled" keyword)
  (cooling-cnt "How many are cooling." fixnum)
  (total-cnt "Total that could be cooling." fixnum))
