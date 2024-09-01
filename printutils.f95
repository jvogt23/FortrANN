! --- Print Utilities for FortrANN --- !
! Written by: James Vogt
! Date: 2024-07-28
! This module contains utilities to print out
! various data structures and other items to the
! standard output. This is used for basic debugging,
! and I have included it as a module because
! printing to stdout in Fortran can be frustrating
! in some applications.

module printutils
    implicit none
    contains
    ! Prints a matrix in a more readable format.
    ! Each new row appears on a new line.
    ! @param arr - An mxn 2D matrix. Should be one-indexed
    subroutine printMatrix(arr)
        real(kind=8), dimension(:,:), allocatable, intent(in) :: arr
        integer :: dimRow, dimCol, i, j
        if (allocated(arr)) then
            dimRow = size(arr, 1)
            dimCol = size(arr, 2)
            do i = 1, dimRow
                write(*, fmt='(a)', advance='no') '['
                do j = 1, dimCol
                    write (*, fmt='(F20.8)', advance='no') arr(i, j)
                end do
                write(*, *) ']'
            end do
        end if
        print *, ''
    end subroutine printMatrix

    ! This method almost immediately deprecates the one above it lol
    ! Same as printMatrix, but doesnt require an allocatable array
    subroutine printMatrixNonAlloc(arr)
        real(kind=8), dimension(:,:), intent(in) :: arr
        integer :: dimRow, dimCol, i, j
        dimRow = size(arr, 1)
        dimCol = size(arr, 2)
        do i = 1, dimRow
            write(*, fmt='(a)', advance='no') '['
            do j = 1, dimCol
                write(*, fmt='(F20.8)', advance='no') arr(i, j)
            end do
            write(*,*) ']'
        end do
        print *, ''
    end subroutine printMatrixNonAlloc

    ! Naive pattern matching algorithm. Please don't use on really big strings, thanks.
    ! @param a - The longer string
    ! @param b - The substring
    ! @param ret - The number to get back.
    subroutine countSubstrings(a, b, ret)
        character(*), intent(in) :: a, b
        integer, intent(out) :: ret
        integer :: i

        !if (len(a) < len(b)) return
        !if (len(a).eq.0.or.len(b).eq.0) return
        do i = 1, len(a) - len(b)
            if (a(i:i+len(b) - 1) == b) ret = ret + 1
        end do

    end subroutine countSubstrings
end module printutils
