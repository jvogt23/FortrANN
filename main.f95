program hello
use printutils
use mod_DenseLayer
use mathy_bois
use CsvFileIO
implicit none
type(DenseLayer) :: layer1
!real, dimension(:,:), allocatable :: testIn
!allocate(testIn(4, 3))
!testIn(1, 1) = 0.7
!testIn(1, 2) = .4
!testIn(1, 3) = .43
!testIn(2, 1) = .234
!testIn(2, 2) = .333
!testIn(2, 3) = -.53
!testIn(3, 1) = -.2
!testIn(3, 2) = .2
!testIn(3, 3) = .4
!testIn(4, 1) = -1
!testIn(4, 2) = .6
!testIn(4, 3) = .4444

!call layer1%init(3, 3)

!call printMatrix(layer1%weights)

!call layer1%forward(testIn)

!call printMatrixNonAlloc(layer1%outputs)

!call printMatrixNonAlloc(layer1%relu_forward())
!call printMatrixNonAlloc(e_raise(layer1%outputs))

character(7) :: str = 'abacab'
character(2) :: str2 = 'ab'
type(CsvFile) :: csv
integer :: numOccurrences = 0
call countSubstrings(str, str2, numOccurrences)
print *, numOccurrences

call csv%openCsvFile('x.csv')

print *, csv%numCol
print *, csv%numRow

!deallocate(testIn)
end program

