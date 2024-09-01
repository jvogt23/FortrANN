module mod_DenseLayer
    use mathy_bois
    implicit none

    type :: DenseLayer
        integer :: nNeurons, nInputs
        real(kind=8), dimension(:,:), allocatable :: weights
        real(kind=8), dimension(:), allocatable :: biases
        real(kind=8), dimension(:,:), allocatable :: outputs
    contains
        procedure :: init
        procedure :: forward
    end type DenseLayer

contains
    ! Initializes a new DenseLayer.
    ! @param nInputs - The number of inputs expected of each neuron
    ! @param nNeurons - The number of neurons in the layer
    subroutine init(self, nInputs, nNeurons)
        class(DenseLayer) :: self
        integer, intent(in) :: nInputs, nNeurons
        integer :: i, j
        real(kind=8), dimension(nInputs, nNeurons) :: tempWeights
        real(kind=8), dimension(nNeurons) :: tempBiases
        self%weights = tempWeights
        self%biases = tempBiases
        self%biases = 0.0
        do i = 1, nInputs
            do j = 1, nNeurons
                self%weights(i, j) = 0.01 * rand_normal()
            end do
        end do
        self%nNeurons = nNeurons
        self%nInputs = nInputs
    end subroutine init

    ! Performs a forward pass through the layer.
    ! @param inputs - A matrix of input values. Must have
    !                 The same number of columns as the rows in weights.
    subroutine forward(self, inputs)
        class(DenseLayer) :: self
        real(kind=8), dimension(:,:), intent(in) :: inputs
        self%outputs = matmul(inputs, self%weights)
    end subroutine forward

end module mod_DenseLayer
