!***************************************************************************
!    This program calculate the radial distribution function from 
!    Chau-made DL_POLY's HISTORY files.
!
!     KM Tu 2011                                                            
!                                                                           
!***************************************************************************

PROGRAM rdf_dlchau
  IMPLICIT NONE
  CHARACTER(LEN=128) :: input_filename, output_filename
  INTEGER, PARAMETER :: input_fileid = 11, output_fileid = 12
  LOGICAL :: is_input_filename_assigned, is_output_filename_assigned
  
  is_input_filename_assigned = .FALSE.
  is_output_filename_assigned = .FALSE.
  
  call get_argument()  

  if (.NOT. is_output_filename_assigned) then
     output_filename = TRIM(ADJUSTL(input_filename)) // ".rdf"
  end if
  
CONTAINS
  SUBROUTINE get_argument()
    IMPLICIT NONE
    INTEGER :: stat, i, n
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 4
    CHARACTER(LEN=128) :: usage, arg

    n = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
    !<column>: the column to be used as data.
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <in file> -r <dr> &
         &[-o <out file>]"

    if (n < LEAST_REQUIRED_NUM_ARG) then
       write(*,*) "Insufficient arguments!"
       write(*,*) usage
       call EXIT(1)
    end if

    i = 1
    do while (i <= n)
       call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
       i = i + 1
       select case (arg)
       case ('-f')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=input_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -f"
             write(*,*) usage
             call EXIT(1)
          end if
          is_input_filename_assigned = .TRUE.

       case ('-o')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=output_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -o"
             write(*,*) usage
             call EXIT(1)
          end if
          is_output_filename_assigned = .TRUE.
          
       case ('-r')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -r"
             write(*,*) usage
             call EXIT(1)
          end if
          read(arg, *, IOSTAT=stat) column
          if (stat /= 0) then
             write(*,*) "Unable to parse the value of argument -r, a&
                  & real number is needed!"
             write(*,*) usage
             call EXIT(1)
          else if (column <= 0) then
             write(*,*) "Illegal value of argument -r, a positive real&
                  & number is needed!"
             call EXIT(1)
          end if
          
       case default
          write(*,*) "Unknown argument: ", arg
          call EXIT(1)
       end select
    end do

    if (.NOT. is_input_filename_assigned) then
       write(*,*) "<in file> (input filename) should be provided."
       write(*,*) usage
       call EXIT(1)
    end if
  END SUBROUTINE get_argument  
END PROGRAM rdf_dlchau
