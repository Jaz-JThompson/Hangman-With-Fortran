        module RandomWordModule
          implicit none
          contains
          subroutine SelectRandomWord(fileName,randomWord)
            implicit none
            character(9), intent(in)   :: fileName
            character(20), intent(out) :: randomWord
            integer                    :: numWords
            real(8)                    :: randomNumber
            integer                    :: randomIndex
            integer                    :: ii, ios

            ! open the file
            open(unit=10,file = fileName, status = 'old', action='read')
            ! 'old' means opening an existing file 

            ! Count the number of words in the file
            numWords = 0 ! initialise no of words
            do 
              read(10, *, iostat=ios) randomWord
            if (ios /= 0) exit
              numWords = numWords + 1 
            end do 

            ! Generate random index
            ! get seed
            call random_seed()
            ! gen random number between [0,1]
            call random_number(randomNumber)
                      
            randomIndex = 1+modulo(int(randomNumber* numWords), numWords)
            ! Modulo gets the remainder of the devisor
            ! e.g. modulo(10,3) = 1 as 10-(3x3)=1

            ! rewind the file
            rewind(10) 

            ! read the random word
            do ii = 1, randomIndex
              read(10, *) randomWord
            end do
            ! close file
            close(10)

          end subroutine SelectRandomWord
        end module RandomWordModule  

