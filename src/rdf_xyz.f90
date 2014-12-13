!***************************************************************************
!    This program calculates the radial distribution function from 
!    data files consisting of xyz coordinates
!
!    Here, we are calculating the g(r) for atom2 around atom1.
!    That is, atom1 are always viewed as the central atoms.
!
!    atom1 and atom2 may be identical atom groups.
!
!     KM Tu 2013                                                            
!                                                                           
!***************************************************************************

PROGRAM rdf_xyz
  IMPLICIT NONE
  CHARACTER(LEN=128) :: output_filename, control_filename
  INTEGER, PARAMETER :: output_fileid = 11, control_fileid = 12
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: data_filename
  INTEGER, DIMENSION(:), ALLOCATABLE :: data_fileid
  
  REAL(KIND=8) :: dr !the thickness of each rdf shell
  REAL(KIND=8) :: r_max !the largest value of r, larger than that g(r) is useless
                        !because atoms will interact with their own images
  LOGICAL :: is_data_filename_assigned

  INTEGER, DIMENSION(:), ALLOCATABLE :: atom_index1, atom_index2
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: atom_index_combined
  REAL, DIMENSION(:,:), ALLOCATABLE :: atom_pos1, atom_pos2
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: is_same_atom

  REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: g !g(r) = rdf
  INTEGER :: nhist !number of histograms of g(r)
  REAL(KIND=8), DIMENSION(3) :: box_dim  

  is_data_filename_assigned = .FALSE.
  
  !default values
  control_filename = "rdf_control"
  output_filename = "rdf_results"
  dr = 0.1

  call read_command()
  call read_control()
  call read_data()
  call output()
  
CONTAINS
  SUBROUTINE read_command()
    IMPLICIT NONE
    INTEGER :: stat, i, j, num_arg, num_data_files
    INTEGER, PARAMETER :: LEAST_REQUIRED_NUM_ARG = 2
    CHARACTER(LEN=128) :: usage, arg

    num_arg = COMMAND_ARGUMENT_COUNT()
    call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=arg)
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
    
    read(UNIT=control_fileid, FMT=*, IOSTAT=stat) dr
    if (stat /= 0) then
       write(*,*) "Unable to read dr in control file: ", TRIM(ADJUSTL(control_filename))
       call EXIT(1)
    end if
    write(*,*) "dr = ",dr

    !read atom_index1
    read(control_fileid, *) num_atom, initial_index, step
    if (stat /= 0) then
       write(*,*) "Unable to read atom1 info in control file: ", TRIM(ADJUSTL(control_filename))
       call EXIT(1)
    end if    
    ALLOCATE(atom_index1(num_atom))
    ALLOCATE(atom_pos1(num_atom, 3))
    do i = 1, num_atom
       atom_index1(i) = initial_index + (i-1)*step
    end do
    write(*,*) "atom index 1:", atom_index1
    
    !read atom_index2
    read(control_fileid, *) num_atom, initial_index, step
    if (stat /= 0) then
       write(*,*) "Unable to read atom2 info in control file: ", TRIM(ADJUSTL(control_filename))
       call EXIT(1)
    end if        
    ALLOCATE(atom_index2(num_atom))
    ALLOCATE(atom_pos2(num_atom, 3))    
    do i = 1, num_atom
       atom_index2(i) = initial_index + (i-1)*step
    end do
    write(*,*) "atom index 2:", atom_index2    
    
    ALLOCATE(is_same_atom(SIZE(atom_index1), SIZE(atom_index2)))
    is_same_atom = .FALSE.
    
    ALLOCATE(atom_index_combined(SIZE(atom_index1) + SIZE(atom_index2), 2))
    call bubble_sort_int(atom_index1, SIZE(atom_index1))
    call bubble_sort_int(atom_index2, SIZE(atom_index2))

    !combine the two indexes in an ascending order
    !the 2nd dimension of atom_index_combined indicates 
    !to which group (1 or 2) this atom_index belongs
    j = 1 !counter for atom_index1
    k = 1 !counter for atom_index2
    do i = 1, SIZE(atom_index_combined(:,1))
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
!write(*,*) "atom_index_combined(:,1) =", atom_index_combined(:,1)
!write(*,*) "atom_index_combined(:,2) =", atom_index_combined(:,2)    
  END SUBROUTINE read_control

  SUBROUTINE read_data()
    IMPLICIT NONE
    INTEGER :: i, j, k, num_atoms, num_frames, dummy_int, current_line, m, n, stat, mark
    REAL(KIND=8) :: dummy_real
    REAL(KIND=8), DIMENSION(3) :: data_real

    do i = 1, SIZE(data_fileid)
       write(*,*) "Start reading data file", i, ": ", TRIM(ADJUSTL(data_filename(i)))
       read(data_fileid(i), *) num_frames, dummy_int, dummy_int, num_atoms
       write(*,*) "num_frames =", num_frames
       write(*,*) "num_atoms =", num_atoms

       !skip 1 line
       read(data_fileid(i), *)
       
       !read the 1st simulation box dimensions
       !to determine boundaries, ie. r_max,  of g(r) histograms
       !--- To Jedrzej ---!
       !Maybe you need to figure out a better way to determine the r_max
       !For example, read all the cell dimension data first to find out
       !the smallest dimension. Make it half you get r_max
       !    r_max = minimum_box_dim / 2.0
       !------------------!
       read(data_fileid(i), *) box_dim(1)
       read(data_fileid(i), *) dummy_real, box_dim(2)
       read(data_fileid(i), *) dummy_real, dummy_real, box_dim(3)

       if (i == 1) then
          r_max = MINVAL(box_dim)/2.0
          nhist = CEILING(r_max/dr)
          ALLOCATE(g(0:nhist))
          !initialize g(r)
          call gr(switch=0)
       end if

       !skip 5 lines
       call skip_lines(data_fileid(i), 5)

       !read frame by frame
       mark = num_frames/100
       do j = 1, num_frames
          !output current progress
          if (MOD(j, mark) == 0) then
             !T2: move output cursor to the 2nd column
             write(UNIT=*, FMT="(1X,'Reading %',I3,' of file ',I3)") &
                  &j/mark, i
          end if
          !read box size for wrapping coords
          read(data_fileid(i), *) box_dim(1)
          read(data_fileid(i), *) dummy_real, box_dim(2)
          read(data_fileid(i), *) dummy_real, dummy_real, box_dim(3)
          
          !read atom records
          m = 1 !counter for atom group 1
          n = 1 !counter for atom group 2
          do k = 1, SIZE(atom_index_combined(:,1))
             if (k > 1 .AND. atom_index_combined(k,1) == atom_index_combined(k-1,1)) then
                !consecutive identical atom_index, no need to read data again
                if (atom_index_combined(k,2) == 1) then
                   atom_pos1(m,:) = data_real
                   m = m + 1
                else
                   atom_pos2(n,:) = data_real
                   n = n + 1
                end if
                is_same_atom(m-1, n-1) = .TRUE.
             else
                if (k==1) then
                   !jump forward to the first atom record to be read
                   call skip_lines(data_fileid(i), atom_index_combined(k,1) - 1)
                else
                   !jump forward to the next atom record to be read
                   call skip_lines(data_fileid(i), atom_index_combined(k,1) - &
                        & atom_index_combined(k-1,1) - 1)
                end if
                read(UNIT=data_fileid(i), FMT=*, IOSTAT=stat) data_real
                if (stat /= 0) then
                   write(*,*) "Error occurs while reading data!"
                   call EXIT(1)
                end if
                if (atom_index_combined(k,2) == 1) then
                   atom_pos1(m,:) = data_real
                   m = m + 1
                else
                   atom_pos2(n,:) = data_real
                   n = n + 1
                end if
             end if
          end do
          !skip the rest unused atom records of the current frame
          call skip_lines(data_fileid(i), num_atoms - MAXVAL(atom_index_combined(:,1)))
          !sample g(r)
          call gr(switch=1)
       end do
    end do
    !determine g(r)
    call gr(switch=2)
  END SUBROUTINE read_data

  SUBROUTINE skip_lines(fileid, n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n, fileid
    INTEGER :: i
    do i = 1, n
       read(fileid, *)
    end do
  END SUBROUTINE skip_lines

  !Caluclate rdf, codes from Frenkel(2002)
  SUBROUTINE gr(switch)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: switch
    INTEGER, SAVE :: ngr
    INTEGER :: i, j, ig, nid
    REAL(KIND=8) :: vb, rho, r
    REAL(KIND=8), DIMENSION(3) :: xr
    REAL(KIND=8), DIMENSION(:), SAVE, ALLOCATABLE :: temp_g
    REAL(KIND=8), PARAMETER :: pi = 3.141592653589793238462643383

    if (switch == 0) then
       if (.NOT. ALLOCATED(temp_g)) then
          ALLOCATE(temp_g(0:SIZE(g)-1))
       end if
       g = 0.0
       ngr = 0
       temp_g = 0.0
    else if (switch == 1) then
       ngr = ngr + 1
       temp_g = 0.0
       do i = 1, SIZE(atom_index1)
          do j = 1, SIZE(atom_index2)
             if (.NOT. is_same_atom(i,j)) then
                xr = atom_pos1(i,:) - atom_pos2(j,:)
                !wrap distance (periodic boundary conditions)
                xr = ABS(xr - box_dim * NINT(xr / box_dim))
                r = SQRT(SUM(xr**2))

                if (r < r_max) then
                   ig = INT(r / dr)
                   temp_g(ig) = temp_g(ig) + 1
                end if
             end if
          end do
       end do
       rho = DBLE(SIZE(atom_index2)) / (box_dim(1) *box_dim(2) * box_dim(3))
       temp_g = temp_g / rho
       g = g + temp_g
    else if (switch == 2) then
       do i = 0, nhist
          !volume between bin i+1 and i
          vb = (4d0/3d0) * pi * ((i+1)**3 - i**3) * dr**3
          g(i) = g(i) / (ngr * SIZE(atom_index1) * vb)
       end do
       DEALLOCATE(temp_g)
    end if
  END SUBROUTINE gr

  SUBROUTINE output()
    IMPLICIT NONE
    INTEGER :: stat, i
    open(UNIT=output_fileid, FILE=output_filename, IOSTAT=stat)
    if (stat /=0) then
       write(*,*) "Unable to open output file: ", TRIM(ADJUSTL(output_filename))
       call EXIT(1)
    end if
    do i = 0, nhist
       write(output_fileid,*) dr*(i + 0.5), g(i)
    end do
  END SUBROUTINE output
END PROGRAM rdf_xyz

SUBROUTINE bubble_sort_int(arr, n)
  IMPLICIT NONE
  INTEGER, DIMENSION(n), INTENT(INOUT) :: arr
  INTEGER :: i, j, temp, n
  
  do i = SIZE(arr)-1, 1, -1
     do j = 1, i
        if (arr(j) > arr(j+1)) then
           temp = arr(j)
           arr(j) = arr(j+1)
           arr(j+1) = temp
        end if
     end do
  end do
END SUBROUTINE bubble_sort_int
