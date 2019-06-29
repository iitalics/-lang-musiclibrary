#lang musiclibrary
(require musiclibrary/extra)
(provide (all-defined-out))

(define 1000-gecs
  (album
   "1000 gecs"
   (artist: "100 gecs")
   ; -------------------------------------
   (sliced-tracks
    #:audio (build-path "./SOURCES/1000-Gecs.m4a")
    "745 Sticky"               0:00
    "Money Machine"            2:21
    "800dB Cloud"              4:15
    "I Need Help Immediately"  6:35
    "Stupid Horse"             7:57
    "xXXi_wud_nvrstøp_ÜXXx"    10:00
    "Ringtone"                 12:54
    "Gecgecgec"                15:18
    "Hand Crushed by a Mallet" 17:42
    "Gec 2 Ü"                  19:50)))

(define exmil
  (album
   "Exmilitary"
   (artist: "Death Grips")
   (sliced-tracks
    #:audio (build-path "./SOURCES/Exmilitary.ogg")
    "Beware"                        0:00
    "Guillotine"                    5:53
    "Spread Eagle Cross The Block"  9:36
    "Lord of the Game"              13:28
    "Takyon (Death Yon)"            16:58
    "Cut Throat (Instrumental)"     19:46
    "Klink"                         20:58
    "Culture Shock"                 24:20
    "5D"                            28:41
    "Thru the Walls"                29:24
    "Known for it"                  33:20
    "I Want it I Need it"           37:33
    "Blood Creepin"                 43:44)))

(define money-store
  (album
   "The Money Store"
   (artist: "Death Grips")
   (sliced-tracks
    #:audio (build-path "./SOURCES/The-Money-Store.ogg")
    "Get Got"              0:00
    "The Fever (Aye Aye)"  2:52
    "Lost Boys"            5:59
    "Blackjack"            9:05
    "Hustle Bones"         11:27
    "I've Seen Footage"    14:30
    "Double Helix"         17:53
    "System Blower"        20:43
    "The Cage"             24:30
    "Punk Weight"          28:01
    "Fuck That"            31:26
    "Bitch Please"         33:50
    "Hacker"               36:47)))

(define clppng
  (album
   "CLPPNG"
   (artist: "Clipping")
   (sliced-tracks
    #:audio (build-path "./SOURCES/CLPPNG.opus")
    "Intro"                             0:00
    "Body & Blood"                      1:05
    "Work Work (ft. Cocc Pistol Cree)"  5:34
    "Summertime (ft. King T)"           09:17
    "Taking Off"                        13:19
    "Tonight (ft. Gangsta Boo)"         18:07
    "Dream"                             22:42
    "Get Up (ft. Mariel Jacoda)"        28:09
    "Or Die (ft. Guce)"                 31:06
    "Inside Out"                        35:12
    "Story 2"                           38:48
    "Dominoes"                          41:00
    "Ends"                              46:58
    "Williams Mix (ft. Tom Erbe)"       51:19)))

(define odyssey
  (album
   "Odyssey"
   (artist: "HOME")
   (sliced-tracks
    #:audio (build-path "./SOURCES/Odyssey.opus")
    "Intro"                             0:00
    "Native"                            3:09
    "Decay"                             7:11
    "Oort Cloud"                        11:22
    "Tides"                             14:48
    "Nights (I Wish I Could Be There)"  18:45
    "Odyssey"                           21:51
    "New Machines"                      28:00
    "Resonance"                         30:58
    "Come Back Down"                    34:31
    "Half Moon"                         39:24
    "On The Way Out"                    43:45)))

(define you-wont-get
  (album
   "You Won't Get What You Want"
   (artist: "Daughters")
   (sliced-tracks
    #:audio (build-path "./SOURCES/You-Wont-Get.ogg")
    "City Song"                0:00
    "Long Road No Turns"       5:55
    "Satan In The Wait"        11:00
    "The Flammable Man"        18:06
    "The Lords Song"           20:16
    "Less Sex"                 23:02
    "Daughter"                 27:50
    "The Reason They Hate Me"  32:45
    "Ocean Song"               36:41
    "Guest House"              44:10)))

(define emerald
  (album
   "Emerald Fantasy Tracks"
   (artist: "Lone")
   (sliced-tracks
    #:audio (build-path "./SOURCES/Emerald-Fantasy-Tracks.m4a")
    "Cloud 909"                     0:00
    "Aquamarine"                    5:30
    "Moon Beam Harp"                10:42
    "Ultramarine"                   14:55
    "Re-Schooling"                  21:09
    "Rissotowe_4"                   24:42
    "Petrcane Beach Track"          30:06
    "The Birds Don't Fly This High" 35:27)))
