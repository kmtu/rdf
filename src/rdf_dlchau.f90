!***************************************************************************
!    This program calculates the radial distribution function from 
!    Chau-made DL_POLY's HISTORY files.
!
!     KM Tu 2011                                                            
!                                                                           
!***************************************************************************

PROGRAM rdf_dlchau
  IMPLICIT NONE
  CHARACTER(LEN=128) :: output_filename, control_filename, atqref_filename
  INTEGER, PARAMETER :: output_fileid = 11, control_fileid = 12, atqref_fileid = 13
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: data_filename
  INTEGER, DIMENSION(:), ALLOCATABLE :: data_fileid
  
  REAL(KIND=8) :: dr !the thickness of each rdf shell
  LOGICAL :: is_data_filename_assigned, is_atqref_filename_assigned

  is_data_filename_assigned = .FALSE.
  is_atqref_filename_assigned = .FALSE.
  
  !default values
  control_filename = "rdf_control"
  output_filename = "rdf_results"
  dr = 0.1

  !check ATQREF
  
  call get_argument_command()
  call get_argument_control()
  
CONTAINS
  SUBROUTINE get_argument_command()
    IMPLICIT NONE
    INTEGER :: stat, i, num_arg, num_data_files
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 4
    CHARACTER(LEN=128) :: usage, arg

    num_arg = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <data file1> -a <ATQREF file>&
         & [<data file2>...] [-c <control file>] [-o <out file>]"

    if (num_arg < LEAST_REQUIRED_NUM_ARG) then
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
          !count number of data files                                      
          num_data_files = 0
          j = i
          do while (.TRUE.)
             j = j + 1
             if (j > num_arg) then
                EXIT
             end if
             call GET_COMMAND_ARGUMENT(NUMBER=j, VALUE=arg, STATUS=stat)
             if (stat /= 0) then
                write(*,*) "Error: unable to read the value of argument -f &
                     &<data file1> [<data file2> ...]"
                write(*,*) usage
                call EXIT(1)
             else if (arg(1:1) == '-') then !end of data file arguments    
                EXIT
             end if
             num_data_files = num_data_files + 1
          end do
          if (num_data_files == 0) then
             write(*,*) "Error: at least one data file must be provided!"
             write(*,*) usage
             call EXIT(1)
          end if
          
          ALLOCATE(data_fileid(num_data_files))
          ALLOCATE(data_filename(num_data_files))
          
          do j = 1, num_data_files
             i = i + 1
             call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=data_filename(j), STATUS=stat)
             if (stat /= 0) then
                write(*,*) "Error: unable to read the value of argument -f &          
                     &<data file1> [<data file2> ...]"
                write(*,*) usage
                call EXIT(1)
             end if
             is_data_filename_assigned = .TRUE.
          end do

       case ('-a')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=atqref_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -a"
             write(*,*) usage
             call EXIT(1)
          end if
          is_atqref_filename_assigned = .TRUE.
          
       case ('-o')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=output_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -o"
             write(*,*) usage
             call EXIT(1)
          end if

       case ('-c')
          call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=control_filename, STATUS=stat)
          i = i + 1
          if (stat /= 0) then
             write(*,*) "Unable to read the value of argument -c"
             write(*,*) usage
             call EXIT(1)
          end if
          
       case default
          write(*,*) "Unknown argument: ", arg
          call EXIT(1)
       end select
    end do

    !check if all the neccessary arguments are given
    if (.NOT. is_data_filename_assigned) then
       write(*,*) "At least one <data file> is needed!"
       write(*,*) usage
       call EXIT(1)
    else if (.NOT. is_atqref_filename_assigned) then
       write(*,*) "ATQREF file is needed!"
       write(*,*) usage
       call EXIT(1)       
    end if
    
    !open every data file                                        
    do i = 1, num_data_files
       data_fileid(i) = 100 + i
       open(UNIT=data_fileid(i), FILE=data_filename(i), IOSTAT=stat, &
            &STATUS="OLD", ACTION="READ")
       if (stat /=0) then
          write(*,*) "Error: unable to open file: ", TRIM(ADJUSTL(data_filename(i)))
          call EXIT(1)
       end if
       write(*,*) "data file ",i ,":", TRIM(ADJUSTL(data_filename(i)))
    end do
  END SUBROUTINE get_argument_command

  SUBROUTINE get_argument_control()
    IMPLICIT NONE
    
  END SUBROUTINE get_argument_control
END PROGRAM rdf_dlchau
