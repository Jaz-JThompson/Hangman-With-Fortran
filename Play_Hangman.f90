        program PlayHangman
          ! ####################################################
          ! Initalise modules for the Hangman game
          ! ####################################################
          use RandomWordModule
          use DrawAsciiHangman
          use GuessLetter


          implicit none
          character(20)               :: word
          integer                     :: wordLen
          character(:), allocatable   :: guessString
          integer                     :: numMistakes
          integer                     :: maxIncorrect
          logical                     :: continueGame
          character(1), dimension(26) :: guess
          integer                     :: ii, trys
          integer, dimension(5)       :: positions
          integer                     :: correctCounter
          ! call random word subroutine to get random word
          call SelectRandomWord('words.txt', word)
          
          ! display random words
         ! print *, "Random Word: ", trim(word)
         ! print *, "Word Length: ", len(trim(word))
          
          ! get details of random word
          wordLen = len(trim(word))
          
          ! initalise incorect count
          numMistakes  = 0
          maxIncorrect = 7
          continueGame = .true.
          correctCounter = 0  
          trys = 0 
          ii = 1

          allocate(character(len=wordLen) :: guessString) 
          guessString = PrintLen(wordLen)

          ! #######################################################
          ! Begin game with graphic
          print *,"-----------------HANGMAN--------------------"
          print *,'           ',  guessString, '            '     
          do while (continueGame .and. numMistakes < maxIncorrect)
             guess(ii) = AskForLetter()
            ! print *, "Guessed letter :", guess(ii)
             positions = GetPositions(word, wordLen, &
                                       guess(ii))
            ! print *, "letter occurs in position: ", positions

             if (all(positions == 0))  then
              ! print *, "INCORRECT TRY AGAIN"

               numMistakes = numMistakes + 1 
               print *,'           ',  guessString, '           '
              
               call AsciiManLevel(numMistakes)
             else
              ! print *, "CORRECT" 
               guessString =  PrintGuess(positions, word, wordLen,&
                                         guess(ii), guessString)
               ! counts the number of correct spaces filled
               correctCounter = CorrectLetterCounter(positions, &
                                word, wordLen, guess(ii),correctCounter)
              
               if (numMistakes /= 0) then
                 call AsciiManLevel(numMistakes)
               end if
                        
             end if
            

             trys  = trys + 1
            ! print *, "Number of letters guessed: ", trys
            if (correctCounter == wordLen) then
              continueGame = .false.
            end if 
            print *,"--------------------------------------------"

          end do

          if (numMistakes >= maxIncorrect) then
            print *, "-----------------GAME OVER-------------------"
            print *, "|| Hidden word:                  ", word
            print *, "|| Number of letters guessed: ", trys
            print *, "---------------------------------------------"
          else
            print *, "-------------------YOU WIN-------------------"
            print *, "|| Hidden word:                  ", word
            print *, "|| Number of letters guessed: ", trys
            print *, "---------------------------------------------"
            print *, " REWARDS: lucky mushroom"
            print *, "    _    "
            print *, "  /  O\  "
            print *, " (_o__ ) "
            print *, "   |_|   "
            print *, "         "
          end if

   


       
          deallocate(guessString) 
        end program PlayHangman
