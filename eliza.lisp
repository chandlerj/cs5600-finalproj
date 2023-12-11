;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

#|
=========================================================
Module: eliza.lisp: 
Description: A version of ELIZA that takes inputs without 
paretheses around them.
Bugs to vladimir kulyukin in canvas
=========================================================
|#

;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defun read-line-no-punct ()
  "Read an input line, ignoring punctuation."
  (read-from-string
    (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                            (read-line))
                 ")")))

(defun punctuation-p (char) (find char ".,;:`!?#-()\\\""))

;;; ==============================

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((i . you) (you . i) (me . you) (am . are) (my . your) (your . my))
          words))

(defparameter *good-byes* '((good bye) (see you) (see you later) (so long)
			    (test)
			    ))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (let* ((input (read-line-no-punct))
           (response (flatten (use-eliza-rules input))))
      (print-with-spaces response)
      (if (member response *good-byes* :test #'equal)
	  (RETURN))))
  (values))

(defun print-with-spaces (list)
  (mapc #'(lambda (x) (prin1 x) (princ " ")) list))

(defun print-with-spaces (list)
  (format t "~{~a ~}" list))

;;; ==============================

(defparameter *eliza-rules*
  '(
    (((?* ?x) hello (?* ?y))      
    (Hello. I am support agent ELIZA. I am here to answer inquiries related to Fedora Linux 38 running the GNOME Desktop Environment. Please feel free to ask me questions related to this environment.))
 
    (((?* ?x) get started(?* ?y))
     (welcome to fedora linux! I have a wealth of resources for getting started including installing packages performing updates and even playing video games. ask me anything and i will answer your questions if i know how to solve your issue.)
     )

    (((?* ?x) computer (?* ?y))
     (Please be as specific as you can about the issue you are experiencing.)
     (It looks like you are talking about computers.) 
     (This might be outside of the scope of my knowledge.
     )
    
    (((?* ?x) update (?* ?y))
     (to perform a system update open the terminal window. once the window is open type sudo dnf update. you will need to enter the password to install updates. your computer will check for updates and prompt you to install available updates. if you press the y key and then press enter your computer will install the available updates.)
     )
     
    (((?* ?x) video games (?* ?y))
     (to install video games on fedora linux you can use the steam client from valve software. first you will want to open a terminal window by pressing the super key. you might know this key as the windows key. after doing this the gnome desktop launchpad will appear. you can now type terminal and select enter. after opening the terminal simply type sudo dnf install steam. a prompt will appear to enter your administrator password. after you enter your password press y to install steam and its dependencies.)
     )
    
    (((?* ?x) how do i install (?* ?y))
     (to install ?y you will want to use the terminal and the dnf command. to open the terminal press the super key. you may know this key as the windows key. after pressing the super key the gnome launchpad will appear and from here you type terminal and press enter. a terminal emulator will appear. we are now ready to install ?y ! at this point type sudo dnf install ?y and a prompt will appear to enter your administrator password. after typing your password a summary of what is going to be installed will print to the terminal. press y and then enter to install ?y and then you should now be able to open ?y from the gnome launchpad the same way we opened the terminal.)
     )

    (((?* ?x) how do i get (?* ?y))
     (to install ?y you will want to use the terminal and the dnf command. to open the terminal press the super key. you may know this key as the windows key. after pressing the super key the gnome launchpad will appear and from here you type terminal and press enter. a terminal emulator will appear. we are now ready to install ?y ! at this point type sudo dnf install ?y and a prompt will appear to enter your administrator password. after typing your password a summary of what is going to be installed will print to the terminal. press y and then enter to install ?y and then you should now be able to open ?y from the gnome launchpad the same way we opened the terminal.)
     )


    (((?* ?x) how do i uninstall (?* ?y))
     (to uninstall ?y you will need to use the dnf package manager through the terminal. to open the terminal press the super key. you may know this key as the windows key. after pressing the super key the gnome launchpad will appear and from here you type terminal and press enter. a terminal emulator will appear. we are now ready to uninstall ?y ! at this point type sudo dnf remove ?y and a prompt will appear to enter your administrator password. after typing your password a summary of what is going to be removed will print to the terminal. press y and then enter to uninstall ?y.)
     )

    (((?* ?x) how do i remove (?* ?y))
     (to uninstall ?y you will need to use the dnf package manager through the terminal. to open the terminal press the super key. you may know this key as the windows key. after pressing the super key the gnome launchpad will appear and from here you type terminal and press enter. a terminal emulator will appear. we are now ready to uninstall ?y ! at this point type sudo dnf remove ?y and a prompt will appear to enter your administrator password. after typing your password a summary of what is going to be removed will print to the terminal. press y and then enter to uninstall ?y.)
     )

    (((?* ?x) wifi (?* ?y))
     (To connect to a wireless network navigate your mouse to the top right corner of the screen and select the system menu. A dropdown menu will open. from here select the button that says not connected. you should be see your wifi network appear. selecting your wifi network will allow you to enter your network password and if you do not see your network this means your network is offline or you are too far away from your access point.)
     )
    
    (((?* ?x) printer (?* ?y))
     (to add a printer you will need to access gnome-settings. to open gnome settings navigate your mouse to the top right corner of the screen and click on the system menu. the system menu has icons like the clock and internet connection status. after clicking the system menu select the gear icon that appears. gnome-settings will now open. on the left side of the settings window select the printer button. the printer settings should now be open. select the lock in the bottom right corner of the window and then enter your administrator password. after unlocking this panel simply select add printer. if your printer is on the same network as your computer it will appear in the list of avialable printers. select your printer and add it to the list of avialable printers. this printer should now be available to use across the entire operating system)
     )

   (((?* ?x) use the internet (?* ?y))
    (to access the internet on fedora linux you can use the included firefox web browser. to open firefox press the super key on your keyboard. from here the gnome spotlight view will open. type firefox and select the firefox web browser icon that appears. you are now ready to browse the web.)
    )

   (((?* ?x) use the internet (?* ?y))
    (to access the internet on fedora linux you can use the included firefox web browser. to open firefox press the super key on your keyboard. from here the gnome spotlight view will open. type firefox and select the firefox web browser icon that appears. you are now ready to browse the web. if you would like to use another web browser please ask me how to install your favorite web browser.)
    )

   (((?* ?x) sorry (?* ?y))
    (Please don't apologize)
    (Apologies are not necessary)
    )

   
   (((?* x) good bye (?* y))
    (good bye)
    )

   )
)
;;; ==============================

