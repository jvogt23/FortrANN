program hello
use printutils
use mod_DenseLayer
use mathy_bois
use CsvFileIO
use activation_fns
implicit none

!Data division
type(DenseLayer) :: layer1
type(DenseLayer) :: layer2
character(7) :: str = 'abacab'
character(2) :: str2 = 'ab'
integer, dimension(:), allocatable :: actuals
type(CsvFile) :: csv
type(SoftMax) :: layer2Activator
type(LossCal) :: loss
type(AccuracyCal) :: acc
type(ReLU) :: layer1Activator
integer :: numOccurrences = 0, i
real(kind=8), dimension(:,:), allocatable :: annInputs
! Best weights and biases found for each layer - used for naive ANN optimization
real(kind=8), dimension(:,:), allocatable :: bestL1Weights
real(kind=8), dimension(:), allocatable :: bestL1Biases
real(kind=8), dimension(:,:), allocatable :: bestL2Weights
real(kind=8), dimension(:), allocatable :: bestL2Biases
real(kind=8) :: bestLoss, bestAccuracy
!Program itself
call countSubstrings(str, str2, numOccurrences)

call csv%openCsvFile('x.csv')
annInputs = csv%dataSet(:, 1:2)
allocate(actuals(size(csv%dataSet, 1)))

do i = 1, size(actuals)
    actuals(i) = int(csv%dataSet(i, 3)) + 1
end do

call layer1%init(2, 3)
bestL1Weights = layer1%weights
bestL1Biases = layer1%biases

call layer2%init(3, 3)
bestL2Weights = layer2%weights
bestL2Biases = layer2%biases

call layer1%forward(annInputs)
call layer1Activator%relu_forward(layer1)
call layer2%forward(layer1Activator%outputs)
call layer2Activator%sm_forward(layer2)

call loss%calcCxeLoss(layer2Activator%outputs, actuals)
call acc%calcAccuracy(layer2Activator%outputs, actuals)

bestLoss = huge(0.0)
bestAccuracy = 0.0

do i = 1, 100000
    layer1%weights = bestL1Weights +&
        (0.05 * rand_normal_mat(size(bestL1Weights, 1), size(bestL1Weights, 2)))
    layer1%biases = bestL1Biases + (0.05 * rand_normal_arr(size(bestL1Biases)))
    layer2%weights = bestL2Weights +&
        (0.05 * rand_normal_mat(size(bestL2Weights, 1), size(bestL2Weights, 2)))
    layer2%biases = bestL2Biases + (0.05 * rand_normal_arr(size(bestL2Biases)))

    call layer1%forward(annInputs)
    call layer1Activator%relu_forward(layer1)
    call layer2%forward(layer1Activator%outputs)
    call layer2Activator%sm_forward(layer2)

    call loss%calcCxeLoss(layer2Activator%outputs, actuals)
    call acc%calcAccuracy(layer2Activator%outputs, actuals)

    if (loss%loss < bestLoss) then
        bestLoss = loss%loss
        bestAccuracy = acc%accuracy
        bestL1Weights = layer1%weights
        bestL2Weights = layer2%weights
        bestL1Biases = layer1%biases
        bestL2Biases = layer2%biases
        print *, "New weights/biases found"
        print *, "Iteration:"
        print *, i
        print *, "Loss:"
        print *, loss%loss
    else
        layer1%weights = bestL1Weights
        layer2%weights = bestL2Weights
        layer1%biases = bestL1Biases
        layer2%biases = bestL2Biases
    end if

end do

print *, "Best Layer 1 biases:"
print *, bestL1Biases
print *, "Best Layer 1 Weights:"
call printMatrixNonAlloc(bestL1Weights)
print *, ""
print *, "Best Layer 2 biases:"
print *, bestL2Biases
print *, "Best Layer 2 Weights:"
call printMatrixNonAlloc(bestL2Weights)

print *, "Loss achieved:"
print *, bestLoss
print *, "Accuracy achieved:"
print *, bestAccuracy

!call printMatrixNonAlloc(layer1%outputs)
!call smAcFn%sm_forward(layer1)
!call loss%calcCxeLoss(smAcFn%outputs, actuals)
!print *, loss%loss
!call acc%calcAccuracy(smAcFn%outputs, actuals)
!print *, acc%accuracy
!call rlu%relu_backward(layer1, smAcFn%outputs)
!call printMatrixNonAlloc(rlu%backPassGradients)
!deallocate(testIn)
end program

