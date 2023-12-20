      module GuessLetter
        implicit none
        public :: AskForLetter, GetPositions

        contains 

        function AskForLetter() result(letter)
          implicit none
          character(1)   :: letter

          ! prompt user input
          write(*,*) "Enter one letter: "
          
          ! read in the letter
          read(*,*) letter
         end function AskForLetter

       ! Prints the underscore of the number of letters in word
       function PrintLen(wordLen) result(guessString)
         character(3)           :: tile 
         integer, intent(in)    :: wordLen
         integer                :: ii
         character((wordLen*3)) :: guessString
         
         tile = ' _ '
         guessString =  repeat(tile, wordLen)

       end function  PrintLen 
      
       ! Function to count the number of correct letters to end game
       function  CorrectLetterCounter(positions, randomWord, wordLen, &
                           guess,  inputCounter)  result(correctCounter)
       character(1), intent(in)          :: guess
       integer, intent(in)               :: wordLen
       character(wordLen), intent(in)    :: randomWord
       integer, dimension(5)             :: positions
       integer                           :: ii
       integer                           :: inputCounter 
       integer                           :: correctCounter
       correctCounter = inputCounter
       do ii = 1, 5
         if (positions(ii) /= 0) then
           correctCounter = correctCounter + 1
         end if
       end do
       end function CorrectLetterCounter


       ! Make sbroutine that takes positions, word and wordlen and 
       ! prints out guesses in the correct position
       function  PrintGuess(positions, randomWord, wordLen, guess, &
                            inputGuessString) result(guessString)
         
         character(1), intent(in)          :: guess
         integer, intent(in)               :: wordLen
         character(wordLen), intent(in)    :: randomWord
         integer, dimension(5)             :: positions
         integer                           :: ii
         character((wordLen*3))            :: inputGuessString
         character((wordLen*3))            :: guessString
         integer                           :: letterPosition
         guessString = inputGuessString
         do ii = 1, 5
           if (positions(ii) /= 0) then
            ! Print *, "print position", positions(ii)
             letterPosition = 3 * (positions(ii) - 1) + 2
             guessString(letterPosition:letterPosition) = guess 
           end if
         end do 
         print *,'           ',  guessString, '           '
       end function  PrintGuess

       ! Get the postions of the guess letter in rand word
      function GetPositions(randomWord,wordLen,guess) result(positions)
         character(1), intent(in)           :: guess
         integer, intent(in)                :: wordLen
         character(wordLen), intent(in)     :: randomWord
         integer, dimension(5)              :: positions
         integer                            :: ii
         integer                            :: numRepeats
 
         ! initialise the number of repeated letters
         numRepeats = 0 
          
        ! print *, randomWord
         ! Check to see if guessed letter is in word
         do ii = 1, len(randomWord)
          ! print *, "check iterator ii: ", ii
           if (randomWord(ii:ii)  ==  guess) then
             !Append he postiton to the positions array
             numRepeats = numRepeats + 1
             positions(numRepeats) = ii
            ! print *, "iterator/letter position: ", ii
            ! print *,"letter being scanned: ", randomWord(ii:ii)
            ! print *, "Position : ", positions 
           end if
         end do
         positions((numRepeats+1):) = 0 
         
         ! Trim the array to get the positions of the repeats
         
        ! print *, "num times letter repeats: ", numRepeats
        ! print *, "letter found in: ", positions 

       end function GetPositions
                
      end module GuessLetter

