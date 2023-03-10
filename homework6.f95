program homework6
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 6
    ! 03/09/2023
    ! This program implements the quicksort algorithm which leverages recursion.
 
    ! Clear memory
    implicit none

    ! Intialize Variables
    character*50 :: InFile
    real, allocatable :: A(:)
    integer :: i, n

    ! Ask user for File input
    print*, 'Enter input file: '
    read(*,*) InFile
    open(42,file=InFile)
    open(13,file=('HW6Out.txt'))

    ! Skip a header line
    read(42,*)
    ! Read in size of file
    read(42,*) n 
    ! Allocate size of array based on file
    allocate(A(n))

    ! Import Array
    do i = 1, n
        read(42,*) A(i)
    enddo

    ! Run Better Bubble Sort algorithm with index sort.
    call QuickSort(A, n, 1, n)

    ! Write header of the file for test suite to work
    write(13,*) 'Sorted Data'
    write(13,*) n

    ! Output Results
    do i = 1, n
        write(13,*) A(i)
    enddo 

    ! Write footer of the file for test suite to work
    write(13,*) 'Done!'

    ! Deallocate Memory
    deallocate(A)

    ! Close the files
    close(13)
    close(42)
       
end program homework6

recursive subroutine QuickSort(A, n, lo, hi)

    ! Clear memory beforehand
    implicit none
    
    ! Declare our variables
    real :: A(n)
    integer :: n, lo, hi, p, Partition

    ! Recursively do our sorting
    if (lo < hi) then
        p = Partition(A, n, lo, hi)
        call QuickSort(A, n, lo, p-1)
        call QuickSort(A, n, p+1, hi)
    endif
  
end subroutine QuickSort

function Partition(A, n, lo, hi) result(P)

    ! Declare our variables
    real :: A(n), Pivot, tmp
    integer :: n, i, lo, hi, p

    ! Perform the Partition portion of the algorithm
    Pivot = A(hi)
    P = lo
        do i = lo, hi - 1
            if (A(i) .le. Pivot) then
                tmp = A(P)
                A(P) = A(i)
                A(i) = tmp
                P = P + 1
            endif
        enddo
    tmp = A(hi)
    A(hi) = A(P)
    A(P) = tmp 
end function 