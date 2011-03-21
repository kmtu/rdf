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
  INTEGER, PARAMETER :: output_fileid = 11, control_fileid = 12!, atqref_fileid = 13
  REAL(KIND=8), PARAMETER :: TRANS_CONST = 2.0**30 !constant for transformin integer data
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: data_filename
  INTEGER, DIMENSION(:), ALLOCATABLE :: data_fileid
  
  REAL(KIND=8) :: dr !the thickness of each rdf shell
  LOGICAL :: is_data_filename_assigned, is_atqref_filename_assigned

  INTEGER, DIMENSION(:), ALLOCATABLE :: atom_index1, atom_index2
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: atom_index_combined
  REAL, DIMENSION(:,:), ALLOCATABLE :: atom_pos1, atom_pos2
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: is_same_atom
  

  is_data_filename_assigned = .FALSE.
!  is_atqref_filename_assigned = .FALSE.
  
  !default values
  control_filename = "rdf_control"
  output_filename = "rdf_results"
  dr = 0.1

  call read_command()
  call read_control()
  call read_data()
  
CONTAINS
  SUBROUTINE read_command()
    IMPLICIT NONE
    INTEGER :: stat, i, j, num_arg, num_data_files
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 2
    CHARACTER(LEN=128) :: usage, arg

    num_arg = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
!     usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <data file1> [<data file2>...]&
!          & -a <ATQREF file> [-c <control file>] [-o <out file>]"
    usage = "Usage: " // TRIM(ADJUSTL(arg)) // " -f <data file1> [<data file2>...]&
         & [-c <control file>] [-o <out file>]"    

    if (num_arg < LEAST_REQUIRED_NUM_ARG) then
       write(*,*) "Insufficient arguments!"
       write(*,*) usage
       call EXIT(1)
    end if

    i = 1
    do while (i <= num_arg)
       call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
       i = i + 1
       select case (arg)
       case ('-f')
          !count number of data files                                      
          num_data_files = 0
          j = i
          do while (.TRUE.)
             if (j > num_arg) then
                EXIT
             end if
             call GET_COMMAND_ARGUMENT(NUMBER=j, VALUE=arg, STATUS=stat)
             j = j + 1             
             if (stat /= 0) then
                write(*,*) "Error: unable to count the number of arguments -f &
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
             call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=data_filename(j), STATUS=stat)
             i = i + 1
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
!     else if (.NOT. is_atqref_filename_assigned) then
!        write(*,*) "ATQREF file is needed!"
!        write(*,*) usage
!        call EXIT(1)       
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
  END SUBROUTINE read_command

  SUBROUTINE read_control()
    IMPLICIT NONE
    INTEGER :: stat, num_atom, initial_index, step, i, j, k
    open(UNIT=control_fileid, FILE=control_filename, IOSTAT=stat, STATUS="OLD", ACTION="READ")
    if (stat /=0) then
       write(*,*) "Error: unable to open file: ", TRIM(ADJUSTL(control_filename))
       call EXIT(1)
    end if
    
    read(control_fileid, *) dr

    !read atom_index1
    read(control_fileid, *) num_atom, initial_index, step
    ALLOCATE(atom_index1(num_atom))
    ALLOCATE(atom_pos1(num_atom, 3))
    do i = 1, num_atom
       atom_index1(i) = initial_index + (i-1)*step
    end do

    !read atom_index2
    read(control_fileid, *) num_atom, initial_index, step
    ALLOCATE(atom_index2(num_atom))
    ALLOCATE(atom_pos2(num_atom, 3))    
    do i = 1, num_atom
       atom_index2(i) = initial_index + (i-1)*step
    end do

    ALLOCATE(is_same_atom(SIZE(atom_index1), SIZE(atom_index2)))
    is_same_atom = .FALSE.
    
    ALLOCATE(atom_index_combined(SIZE(atom_index1) + SIZE(atom_index2), 2))
    call bubble_sort_int(atom_index1)
    call bubble_sort_int(atom_index2)
    !combine the two indexes in an ascending order
    j = 1
    k = 1
    do i = 1, SIZE(atom_index_combined)
       if (j <= SIZE(atom_index1) .AND. k <= SIZE(atom_index2)) then
          if (atom_index1(j) <= atom_index2(k)) then
             atom_index_combined(i, 1) = atom_index1(j)
             atom_index_combined(i, 2) = 1
             j = j + 1
          else
             atom_index_combined(i, 1) = atom_index2(k)
             atom_index_combined(i, 2) = 2
             k = k + 1
          end if
       else if (j > SIZE(atom_index1)) then
          atom_index_combined(i, 1) = atom_index2(k)
          atom_index_combined(i, 2) = 2
          k = k + 1
       else if (k > SIZE(atom_index2)) then
          atom_index_combined(i, 1) = atom_index1(j)
          atom_index_combined(i, 2) = 1
          j = j + 1
       end if
    end do
  END SUBROUTINE read_control

  SUBROUTINE read_data()
    IMPLICIT NONE
    INTEGER :: i, j, k, num_atoms, num_frames, dummy_int, current_line, m, n
    REAL(KIND=8) :: dummy_real
    REAL(KIND=8), DIMENSION(3) :: box_dim
    INTEGER, DIMENSION(3) :: data_int
    REAL(KIND=8), DIMENSION(3) :: data_real
    do i = 1, SIZE(data_fileid)
       read(data_fileid(i), *) num_frames, dummy_int, dummy_int, num_atoms
       !skip 9 lines
       do j = 1, 9
          read(data_fileid(i), *)
       end do
       !read frame by frame
       do j = 1, num_frames
          !read simulation box dimensins
          read(data_fileid(i), *) box_dim(1)
          read(data_fileid(i), *) dummy_real, box_dim(2)
          read(data_fileid(i), *) dummy_real, dummy_real, box_dim(3)
          !read atom records
          current_line = 1
          m = 1
          n = 1
          do k = 1, SIZE(atom_index_combined)
             if (k > 1 .AND. atom_index_combined(k,1) == atom_index_combined(k-1,1)) then
                if (atom_index_combined(k,2) == 1) then
                   atom_pos1(m,:) = data_real
                   m = m + 1
                else
                   atom_pos2(n,:) = data_real
                   n = n + 1
                end if
                is_same_atom(m-1, n-1) = .TRUE.
             else
                call skip_lines(data_fileid(i), atom_index_combined(k,1) - current_line)
                read(data_fileid(i), *) data_int
                data_real = DBLE(data_int)*5.0d2/TRANS_CONST
                if (atom_index_combined(k,2) == 1) then
                   atom_pos1(m,:) = data_real
                   m = m + 1
                else
                   atom_pos2(n,:) = data_real
                   n = n + 1
                end if
             end if
          end do
          !calculate radial distribution number
          call rdn(atom_pos1, atom_pos2, dr, ***use structure****)
       end do
    end do
  END SUBROUTINE read_data

  SUBROUTINE skip_lines(fileid, n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n, fileid
    INTEGER :: i
    do i = 1, n
       read(fileid, *)
    end do
  END SUBROUTINE skip_lines
END PROGRAM rdf_dlchau

SUBROUTINE wrap_coords(coords, bounds)
  !This subroutine limit the coords to lie inside values between 0 and bounds
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(:), INTENT(INOUT) :: coords
  REAL(KIND=8), DIMENSION(:), INTENT(IN) :: bounds
  INTEGER :: i
  
  if (SIZE(coords) /= SIZE(bounds)) then
     write(*,*) "Wrapping error:"
     write(*,*) "   Dimension of coords =", SIZE(coords)
     write(*,*) "   Dimension of bounds =", SIZE(bounds)
     write(*,*) "They should be the same!"
     call EXIT(1)
  end if

  do i = 1, SIZE(coords)
     if (coords(i) >= bounds(i)) then
        coords(i) = coords(i) - bounds(i)
     end if
  end do
END SUBROUTINE wrap_coords

SUBROUTINE bubble_sort_int(arr)
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: arr(:)
  INTEGER :: i, j, temp

  do i = SIZE(arr), 1, -1
     do j = 1, i
        if (arr(j) > arr(j+1)) then
           temp = arr(j)
           arr(j) = arr(j+1)
           arr(j+1) = temp
        end if
     end do
  end do
END SUBROUTINE bubble_sort_int
