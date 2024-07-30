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
        implicit none
        real, dimension(:,:), allocatable :: arr
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
        real, dimension(:,:) :: arr
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
end module printutils
