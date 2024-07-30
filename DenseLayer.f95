module mod_DenseLayer
    use mathy_bois
    implicit none

    type :: DenseLayer
        integer :: nNeurons, nInputs
        real, dimension(:,:), allocatable :: weights
        real, dimension(:), allocatable :: biases
        real, dimension(:,:), allocatable :: outputs
    contains
        procedure :: init
        procedure :: forward
        procedure :: relu_forward
    end type DenseLayer

contains
    ! Initializes a new DenseLayer.
    ! @param nInputs - The number of inputs expected of each neuron
    ! @param nNeurons - The number of neurons in the layer
    subroutine init(self, nInputs, nNeurons)
        class(DenseLayer) :: self
        integer, intent(in) :: nInputs, nNeurons
        integer :: i, j
        real, dimension(nInputs, nNeurons) :: tempWeights
        real, dimension(nNeurons) :: tempBiases
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
        real, dimension(:,:), intent(in) :: inputs
        self%outputs = matmul(inputs, self%weights)
    end subroutine forward

    ! ReLU activation function - returns a set of outputs
    ! rectified such that any output value in self%outputs
    ! which is <= 0 is set to 0.
    ! @return mxn matrix of same size as outputs
    function relu_forward(self)
        class(DenseLayer) :: self
        integer :: i, j
        real, allocatable :: relu_forward(:,:)
        allocate(relu_forward(size(self%outputs, 1), size(self%outputs, 2)))
        do i = 1, size(self%outputs, 1)
            do j = 1, size(self%outputs, 2)
                relu_forward(i, j) = max(self%outputs(i, j), 0.0)
            end do
        end do
    end function relu_forward

end module mod_DenseLayer
