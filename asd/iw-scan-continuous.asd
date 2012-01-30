
(defsystem :iw-scan-continuous
  :depends-on (:cl-fad :j-string-utils :iw-scan :alexandria)
  :description "Continuous scanning with iw-scan. Provides a hook to look at
 all the wonderful routers seen. The hooks are just functions with arguments
 result, interface found on.
Nonexhaustive list of hooks.(Use appropos/the autodocs TODO those not actually made yet.)

iw-look-chain:      Convenience to use multiple hooks at the same time.
iw-store-sightings: Store first sightings in files.
iw-look-interests:  Allows you to say what you care about, takes as argument
 another hook which takes the interface and those things as argument.
iw-store-interests: Uses the previous to store stuff you indicate. 
                    TODO memory versus disc storage, quick recall.

TODO: a library compresses slightly changing data with easy time/other based
 access, and another library that makes this useful for iw-scan."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "iw-scan-continuous")))))
