        module DrawAsciiHangman
          implicit none
          contains
          subroutine AsciiManLevel(numMistakes)
            implicit none
            integer                     :: numMistakes
            character(80), dimension(7) :: hangmanStages
            integer                     :: ii

            ! initalise Ascii art using array
            hangmanStages(1) = '  +---+' 
            hangmanStages(2) = '  |   |'
            hangmanStages(3) = '  O   |'
            hangmanStages(4) = ' /|\  |'
            hangmanStages(5) = ' / \  |'
            hangmanStages(6) = '      |'
            hangmanStages(7) = '======='

            do ii = 1, numMistakes
              print *, hangmanStages(ii)
            end do 
          end subroutine AsciiManLevel
        end module DrawAsciiHangman
