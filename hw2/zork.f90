!!!!!!!!!!!!!!!!

! This program is a Zork-esque game that gives the player a series of decision points that affect the overall progression of the game. 
! Jacob Buckelew and Griffin Lehrer, CMS495


!!!!!!!!!!!!!!!!

Program Zork_Game

    implicit none

! Declarations
    character(len = 50):: userInput

! Begin game

    Write(*,*) "Fuse the Seer: Wake up fucking cockroach!"
    Write(*,*) "Its the first day of fucking class and you haven't even read the syllabus!!"
    Write(*,*) " Type 1 to be like TJ and talk smack. Type 2 to be like Lauren and sit quietly."

    read(*, '(A)') userInput
    
    write(*, '(A)') "You chose", userInput

    ! Decision 1: TJ choice
    if(userInput .eq. '1') then
        write(*,*) "You tell Fuse, 'Bitch you trying to tell me what to do. You wanna throw down?'"
        write(*,*) "'Yo mama so fat she got her own gravitational pull.'"
        write(*,*) "Fuse walks over to a chair calmly and picks it up."
        write(*,*) "Fuse then launches the chair at your noggin. EVERYTHING GOES BLACK"
        write(*,*) "You wake up in a hospital bed. Dr. Lavder approaches you with fresh lemon cranberry scones."
        write(*,*) "Type 1 to take a scone. Type 2 to reject a scone."


        read(*,*) userInput
        write(*,*) "You chose", userInput

        ! Decision 2: Take scone choice

        if(userInput .eq. '1') then
            write(*,*) "You take a bite out of a scone. You feel a sudden rush of energy."
            write(*,*) "You can feel movement across the room."
            write(*,*) "Type 1 to go on a rampage. Type 2 to jump out of the window."

            read(*,*) userInput
            write(*,*) "You chose", userInput

            ! Decision 3: Go on a rampage
            if(userInput .eq. '1') then
                write(*,*) "You grab Dr. Lavder with your newfound telekinetic powers and launch her into orbit."
                write(*,*) "You suddenly wonder if she will develop her own elliptical orbiting pattern in space."
                write(*,*) "Outside of the hospital, you wreck havoc on the streets of Gotham City."
                write(*,*) "Suddenly, something sharp pins you onto the wall and notice that it is a batarang."
                write(*,*) "Batman yells, 'Stop at once fool or face my wrath.'"
                write(*,*) "Type 1 to submit to Batman. Type 2 to resist justice."

                read(*,*) userInput
                write(*,*) "You chose", userInput

                ! Decision 4: Submit to Batman
                if(userInput .eq. '1') then
                    write(*,*) "Batman takes you into custody within his Batcave and assigns Alfred to watch over you."
                    write(*,*) "Alfred looks at you coldly and tells you, 'One of those victims you injured was my nephew."
                    write(*,*) "He is now in a hospital receiving 3 stitches for a microscopic scratch on his forehead."
                    write(*,*) "If Batman won't bring justice, then I will do so myself."
                    write(*,*) "Alfred charges up his pupils in order to fire laser beams at your head."
                    write(*,*) "'Ah, I was Superman all along and was sent here for a bounty on TJ."
                    write(*,*) "The world slows down as you watch laser beams slowly approach you in 3D."
                    write(*,*) "It reminds you of going to movie theatres. However your movie is now over."
                    write(*,*) "As the world fades to black, the Spirit of Dr. Fuse appears."
                    write(*,*) "Better luck next time, TJ."
                
                ! Decision 4: Resist justice
                else if(userInput .eq. '2') then
                    write(*,*) "Your body starts to run low on lemon cranberry scone energy."
                    write(*,*) "Your powers slowly fade away and you feel boring again. Batman stares you down."
                    write(*,*) "With the last bit of lemon cranberry scone in your stomach, you force pull a baby to you."
                    write(*,*) "You throw the baby off the roof of the hospital, and Batman suddenly dives off the roof."
                    write(*,*) "As you walk away, you hear a giggle behind you. It's none other than the Joker."
                    write(*,*) "'My daughter, Lauren, join me and we shall terrorize Gotham City together."
                    write(*,*) "You suddenly transform into a gigachad. Congratulations, Lauren, you've won."
                end if


            ! Decision 3: jump out of the window
            else if (userInput .eq. '2') then
                write(*,*) "You burst out of the hospital windows at the speed of light and hit the ground beneath you."
                write(*,*) "With a loud BANG, you crash into the middle of a zombie apocalypse."
                write(*,*) "You are surrounded by flesh-eating, mindless abominations seeking to eat your guts."
                write(*,*) "Type 1 to face the zombies. Type 2 to run like hell."

                read(*,*) userInput 
                write(*,*) "You chose", userInput

                !Decision 4: face the zombies

                if(userInput .eq. '1') then
                    write(*,*) "You storm ahead into a crowd of zombies as if you have no brain yourself."
                    write(*,*) "You take a few swings but get knocked the ground. You are outnumbered 10000:1."
                    write(*,*) "The zombies take out their forks and knives and sprinkle salt and pepper onto you."
                    write(*,*) "Your earlobe is true delicacy for the zombies. The zombies prepare for their 2nd course."
                    write(*,*) "They say grace and you realize everything is over. The world fades to black."
                    write(*,*) "The Spirit of Dr. Fuse approaches you and yells, 'Really TJ, are you that dumb?'"
                    write(*,*) "Should've just read the syllabus."

                ! Decision 4: Run like hell
                else if (userInput .eq. '2') then
                    write(*,*) "You run off as fast you can and see a small camp off in the distance."
                    write(*,*) "The survivors arm you with molotov cocktails and you fight back against the zombie horde."
                    write(*,*) "Although you are outnumbered, you shrink their numbers quickly with your molotov bombs."
                    write(*,*) "You are a hero amongst the survivors. They choose you to be head of the Sanctuary."
                    write(*,*) "Dr. Syrrmed approaches you, 'Lauren, please accept this gift of gratitude."
                    write(*,*) "You open the box and find a blue sweater vest."
                    write(*,*) "In the distance, the spirit of Dr. Fuse nods approvingly, 'What a gigachad.'"
                endif
            endif
            
        ! Decision 2: Reject scone choice

        else if (userInput .eq. '2') then
            write(*,*) "Dr. Lavder looks at you as her eyes water. Guilt consumes you in the moment."
            write(*,*) "Dr. Lavder transforms into a penguin-sized Cthulhu"
            write(*,*) "Cthulhu lunges at you and squeezes you with his MASSIVE tentacles"
            write(*,*) "your guts burst out of your body and the world slowly fades to black"
            write(*,*) "The spirit of Dr. Fuse appears. You have died better luck next time TJ."

        endif

        
    ! Decision 1: Lauren Choice
    else if(userInput .eq. '2') then
        write(*,*) "Ok bitch, that is what I thought. Now get out of my domain!"
        write(*,*) "You walk out of the room. You notice a tall, slender man in a familiar blue sweater vest."
        write(*,*) "It is obviously Dr. Syrrmed. The professor who failed you in CMS167+Lab for naming all your variables fuck."
        write(*,*) "He gives you a petrifying look and points at you. Your heart quivers."
        write(*,*) "Type 1 to approach Dr. Syrrmed. Type 2 to escape to the the Seer's Lair, Room 309."

        read(*,*) userInput
        write(*,*) "You chose", userInput

        ! Decision 2: Approach Syrrrmed choice
        if(userInput .eq. '1') then
            write(*,*) "You walk up and tell Dr. Syrrmed, 'I have a bone to pick with you sir.'" 
            write(*,*) "'I was told i would be watching 'Sorting out Sorting' in CMS 170 but you did not deliver on your promise."
            write(*,*) "Dr. Syrrmed pauses and gives you a sideways glance."
            write(*,*) "'Movies on the first day? Should've expected less from you.'"
            write(*,*) "Type 1 to force choke Dr. Syrrmed. Type 2 to perform a jedi mind trick."

            read(*,*) userInput
            write(*,*) "You chose", userInput

            ! Decision 3: Force Choke
            if(userInput .eq. '1') then
                write(*,*) "You open your palm and make a sqeezing motion at Dr. Syrrmed."
                write(*,*) "You can feel the hatred flowing through your veins and the world seems a little smaller"
                write(*,*) "Your vision fades to red and you can hear the harsh breathing coming from your respirator"
                write(*,*) "Dr. Syrremd look at you in horror please spare my life Lord Vader"
                write(*,*) "You tried to defy ME! I find your lack of faith disturbing Syrrmed."
                write(*,*) "Type 1 to end his life. Type 2 to force push him against the wall"

                read(*,*) userInput
                write(*,*) "You chose", userInput

                ! Decision 4: End his life
                if(userInput .eq. '1') then
                    write(*,*) "You watch as the life slowly fades from Syrrmeds eyes"
                    write(*,*) "This gives you an enourmous amout of pleaure and you can feel the power of the dark side take root & 
                    & in your soul"
                    write(*,*) "Darth Fuseous floats up from the floor and peers into the dark side energy with glee"
                    write(*,*) "You have done well my apprentice. By taking your first life you have taken your first step to & 
                    & transforming into a gigachad"
                    write(*,*) "Congratulations Lauren on taking your first step to becomming darth gigachad you win"

                
                ! Decision 4: Spare his life and force push him against the wall
                else if(userInput .eq. '2') then
                    write(*,*) "You fling Dr. Syrrmed into a wall and he hits a spike spitting in half"
                    write(*,*) "The two halves of Dr. Syrrmed slowly bulge outward and become identical copies of the original Dr. &
                    & Syrrmed"
                    write(*,*) "These copies continue to replicate until there are hundreds of Dr. Syrrmeds"
                    write(*,*) "Unfortunately these copies are programmed to be ninja terminators"
                    write(*,*) "They all simultaneously look at you and drop kick your asshole"
                    write(*,*) "In tremendous pain where the sun don't shine you kneel over and scream"
                    write(*,*) "The ghost of Dr. Fuse comes up and says:"
                    write(*,*) "You have been so embarassed I refuse to continue this game TJ"

                endif

            ! Decision 3: Jedi Mind Trick
            else if(userInput .eq. '2') then
                write(*,*) "We WILL watch this movie today Syrrmed, you say as you wave your meaty paw in Syrrmed's face"
                write(*,*) "Dr. Syrrmed looks at you incredulously. Did you forget to say Dr!!!!!"
                write(*,*) "Dr. Syrrmed slowly grows larger as his lips pull back in a snarl. You realize you fucked up."
                write(*,*) "Dr. Syrrmed slowly transforms into Darth Vader. PUNY JEDI WANNABE YOU SHOULD HAVE FORCE CHOKED ME"
                write(*,*) "HERE IS THE TRUE POWER OF THE DARK SIDE OF THE FORCE"

                write(*,*) "You feel a pressure come over your neck. It feels like the breath is being squeeZed out of your body"
                WRITE(*,*) "Everything goes black"
                write(*,*) "Dr. Fuse ghost: you have died good luck next time TJ"

                STOP

            endif
            
        ! Decision 2: Escape into lair choice
        else if(userInput .eq. '2') then
            write(*,*) "You powerwalk to the lair. However halfway there you notice Dr. Syrrmed following you quickly"
            write(*,*) "What is that you've got in your pocket lad???? Is that a syllabus I see???"
            write(*,*) "Type 1 to answer affirmative and type 2 to answer negatory."

            read(*,*) userInput
            write(*,*) "You chose", userInput

            ! Decision 3: Affirmative choice
            if(userInput .eq. '1') then
                write(*,*) "'Yes, this is indeed the syllabus for your class, Dr. Syrrmed."
                write(*,*) "Gooooooood, and why don't you read it for me now?"
                write(*,*) "Type 1 to read the syllabus. Type 2 to decline."

                read(*,*) userInput
                write(*,*) "You chose", userInput

                !Decision 4: Read syllabus

                if(userInput .eq. '1') then
                    write(*,*) "You read the syllabus out loud and find you are transcending as a human being."
                    write(*,*) "The syllabus captures your entire attention. You blink."
                    write(*,*) "After what feels like a microsecond, you sense a bright light shining towards you."
                    write(*,*) "Annoyed, you try to block the light with your syllabus."
                    write(*,*) "However you realize the light will only keep annoying you no matter what you do."
                    write(*,*) "Furious, you put your syllabus down and find yourself wearing a graduation gown."
                    write(*,*) "The Spirit of Dr. Fuse appears, 'Congratulations Lauren, you now have a PhD in Astrophysics."
                    write(*,*) "Welcome to the big leagues, gigachad."
                
                ! Decision 4: decline
                else if(userInput .eq. '2') then
                    write(*,*) "Dr. Syrrmed, the hermit that he is, slowly walks off and accepts his defeat."
                    write(*,*) "You feel an ego rush but suddenly you see a tentacle slide into the Seer's lair."
                    write(*,*) "The building begins to crumble around and you hear what sounds like the call of Cthulhu."
                    write(*,*) "Dr. Lavder, having just finished a grueling 2 and a half hours of Senior Capstone &
                    & finishes her transformation into Cthulhu and ravages the world around you."
                    write(*,*) "Tentacles squeeze you into pieces as the world fades to black."
                    write(*,*) "The Spirit of Dr. Fuse appears, 'TJ, don't you know that lying is bad?"
                    write(*,*) "I hope this is a lesson for you in the afterlife."
                endif
            
            ! Decision 3: Negatory choice
            else if(userInput .eq. '2') then 
                write(*,*) "Dr. Syrrmed looks puzzled. 'Are you sure you're not lying to me?"
                write (*,*) "That paper does bear resemblance to my class's syllabus."
                write (*,*) "Type 1 to admit that you were lying. Type 2 to stick to your guns."

                read(*,*) userInput
                write(*,*) "You chose", userInput

                ! Decision 4: Admit

                if(userInput .eq. '1') then
                    write(*,*) "Ha, I caught you in the act of deceit. This is unacceptable behavior."
                    write(*,*) "I must bring this up to the Honor Council and you will be expelled from our institution."
                    write(*,*) "The Honor Council eventually finds you guilty of your actions."
                    write(*,*) "Dr. Syrrmed gets the honor of being able to throw you into a river."
                    write(*,*) "As you begin to float, you realize you are actually a witch."
                    write(*,*) "You grab the nearest log, cast a spell on it, and immediately fly off into the pale moonlight."
                    write(*,*) "Dr. Fuse's face appears on the moon, and looks on in approval."
                    write(*,*) "Dr. Fuse casts Selene's Blessing unto you, 'Go forth and prosper, Lauren. Be the gigachad you &
                    & were always meant to be."
                
                ! Decision 4: Stick to your guns
                else if(userInput .eq. '2') then
                    write(*,*) "'Absolutely not. This is not your syllabus, Dr. Syrrmed."
                    write(*,*) "'Ok'. Dr Syrrmed walks off and disappears into the shadows of the night."
                    write(*,*) "A horrible sense of guilt consumes you. You tell yourself, 'I should have told the truth'"
                    write(*,*) "You feel so terrible that you change your major to underground basketweaving and move &
                    & to a deserted island to live a horrible, yet peaceful and exotic life."
                    write(*,*) "During a nightly yoga session by the campfire, the Spirit of Dr. Fuse appears in the flames."
                    write(*,*) "My son, TJ, you are bad."
                endif
            endif

        endif


    endif

    
    Print *, "THE END"


    stop
    END