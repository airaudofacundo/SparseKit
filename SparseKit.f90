!***************************************************
!       Instituto Universitario Aeronautico
!       Dpto. Mecanica Aeronautica
!***************************************************
! Filename      : SparseKit.f90
! Version       : 0.1
! Date          : 07-08-2019
! Programmer(s) : F. Airaudo(fairaudo574@alumnos.iua.edu.ar)
!***************************************************
! Description (brief):
!                     Sparseasdsajkdfopaskfa
!*************************************************** 
! Dependecies:
!             Use tools - Filename: Utilities.f90
!             Use QuickSortMod - Filename: quicksort.f90
!***************************************************
! Public procedures:
!                   Type(Sparse)
!                   
!***************************************************
module SparseKit
  !***********************************************
  !*                 EXTERNS                     *
  !***********************************************
  use tools
  use quickSortMod
  implicit none
  private
  public :: Sparse, operator(*), operator(+), transpose, norm
  type Triplet
     real(dp), dimension(:), allocatable :: A
     integer, dimension(:), allocatable :: row
     integer, dimension(:), allocatable :: col
  end type Triplet
  type Sparse
     private
     real(dp), dimension(:), allocatable :: A
     integer, dimension(:), allocatable :: AI
     integer, dimension(:), allocatable :: AJ
     integer, dimension(:), allocatable :: rowCounter
     integer :: n
     integer :: nnz
     integer :: counter
     type(triplet) :: triplet
   contains
     procedure, public :: init
     procedure, public :: update
     procedure, public :: append
     procedure, public :: makeCRS
     
     procedure, public :: get
     
     procedure, public :: printValue
     procedure, public :: printNonZeros
     procedure, public :: printAll

     procedure, public :: deleteRowAndCol
     
     procedure, private :: handleDuplicates
  end type Sparse

  interface operator(*)
     module procedure sparse_sparse_prod
     module procedure sparse_vect_prod
  end interface operator(*)
  interface operator(+)
     module procedure sparse_sparse_add
  end interface operator(+)
  interface sparse
     procedure constructor
  end interface sparse
  interface transpose
     module procedure transpose
  end interface transpose
  interface norm
     module procedure norm
  end interface norm
  
  
  !***********************************************
  !*          LOCAL PRIVATE VARIABLES            *
  !***********************************************
  logical :: isCRSDone = .false.
  real(dp), dimension(:), allocatable :: valueVector
  real(dp), dimension(:), allocatable :: auxA
  integer, dimension(:), allocatable :: auxAJ
  integer, dimension(:), allocatable :: rowVector
  integer :: repeats, l
  
contains
  !***************************************************
  ! Constructor:
  !     Initializes the object with and estimate of
  !     the matrix's non zeros and the number of rows
  !     Won't work if given an inferior nnz
  !  
  ! Parameters:
  !     Input, nnz(int), rows(int)
  !     Output, -
  !***************************************************
  type(Sparse) function constructor(nnz, rows)
    implicit none
    integer, intent(in) :: nnz
    integer, intent(in) :: rows
    call constructor%init(nnz, rows)
  end function constructor
  subroutine init(this, nnz, rows)
    implicit none
    class(Sparse), intent(InOut) :: this
    integer, intent(in) :: nnz
    integer, intent(in) :: rows
    this%nnz = nnz
    this%n = rows
    allocate(this%triplet%A(this%nnz))
    allocate(this%triplet%row(this%nnz))
    allocate(this%triplet%col(this%nnz))
    this%triplet%A = 0
    this%triplet%row = 0
    this%triplet%col = 0
    this%counter = 0
  end subroutine init
  !***************************************************
  ! update:
  !     updates basic values
  !  
  ! Parameters:
  !     Input, nnz(int), rows(int)
  !     Output, -
  !***************************************************
  subroutine update(this, nnz, rows)
    implicit none
    class(sparse), intent(inout) :: this
    integer, intent(in) :: nnz
    integer, intent(in) :: rows
    isCRSDone = .false.
    if(allocated(this%triplet%A)) then
       deallocate(this%triplet%A, this%triplet%row, this%triplet%col)
    end if
    if(allocated(this%A)) then
       deallocate(this%A, this%AI, this%AJ)
    end if
    this%nnz = nnz
    this%n = rows
    allocate(this%triplet%A(this%nnz))
    allocate(this%triplet%row(this%nnz))
    allocate(this%triplet%col(this%nnz))
    this%triplet%A = 0
    this%triplet%row = 0
    this%triplet%col = 0
    this%counter = 0
  end subroutine update
  !***************************************************
  ! append:
  !     takes values one by one and appends it to a
  !     triplet format
  !  
  ! Parameters:
  !     Input, value(realdp), row(int), col(int)
  !     Output, -
  !***************************************************
  subroutine append(this, value, row, col)
    implicit none
    class(Sparse), intent(InOut) :: this
    class(*), intent(In) :: value
    integer, intent(In) :: row, col
    this%counter = this%counter + 1
    this%triplet%A(this%counter) = value
    this%triplet%row(this%counter) = row
    this%triplet%col(this%counter) = col
  end subroutine append
  !***************************************************
  ! makeCRS:
  !     Once all values have been appended to the
  !     triplet call this routine to make the CRS
  !  
  ! Parameters:
  !     Input, -
  !     Output, CRS is usable
  !***************************************************
  subroutine makeCRS(this)
    implicit none
    class(Sparse), intent(InOut) :: this
    integer :: i
    isCRSDone = .true.
    allocate(this%rowCounter(this%n))
    !This%Counter entries in each row, including duplicates
    this%rowCounter = 0
    do i = 1, this%counter
       this%rowCounter(this%triplet%row(i)) = this%rowCounter(this%triplet%row(i)) + 1
    end do
    !Allocate auxA and auxAJ with nnz
    allocate(auxA(this%counter), auxAJ(this%counter))
    !Order A and AJ
    call quicksort(this%triplet%row, this%triplet%col, this%triplet%A, 1, this%counter)
    !sum up duplicates
    call this%handleDuplicates()
    !Allocate A and AJ with nnz
    allocate(this%A(this%counter), this%AJ(this%counter))
    do i = 1, this%counter
       this%A(i) = auxA(i)
       this%AJ(i) = auxAJ(i)
    end do
    !Counstruct row pointers
    allocate(this%AI(this%n+1))
    this%AI = 1
    do i = 2, this%n+1
       this%AI(i) = this%AI(i-1) + this%rowCounter(i-1)
    end do
    deallocate(this%triplet%A, this%triplet%row, this%triplet%col)
    deallocate(auxA, auxAJ)
    deallocate(this%rowCounter)
    this%nnz = size(this%AJ)
  end subroutine makeCRS
  !***************************************************
  ! handleDuplicates:
  !     Sums up every duplicate on the triplet, also
  !     orders values in each row
  !  
  ! Parameters:
  !     Input, -
  !     Output, -
  !***************************************************
  subroutine handleDuplicates(this)
    implicit none
    class(Sparse), intent(InOut) :: this
    logical :: mask
    integer :: i, j, k
    allocate(rowVector(maxval(this%rowCounter)))
    allocate(valueVector(maxval(this%rowCounter)))
    this%counter = 1
    repeats = 0
    do i = 1, this%n
       rowVector = this%triplet%col(this%counter+repeats:this%counter+repeats+this%rowCounter(i)-1)
       valueVector = this%triplet%A(this%counter+repeats:this%counter+repeats+this%rowCounter(i)-1)
       call quicksort(rowVector, valueVector, 1, this%rowCounter(i))
       j = 0
       do while(j < this%rowCounter(i))
          j = j + 1
          auxA(this%counter) = valueVector(j)
          auxAJ(this%counter) = rowVector(j)
          k = j
          do while(k < this%rowCounter(i))
             k = k + 1
             mask = rowVector(j).eq.rowVector(k)
             if(mask) then
                !add values from k to j
                auxA(this%counter) = auxA(this%counter) + valueVector(k)
                !move k to the back
                call swap(rowVector(k), rowVector(this%rowCounter(i)))
                call swap(valueVector(k), valueVector(this%rowCounter(i)))
                this%rowCounter(i) = this%rowCounter(i) - 1
                repeats = repeats + 1
             end if
          end do
          this%counter = this%counter + 1
       end do
    end do
    this%counter = this%counter - 1
    deallocate(rowVector)
    deallocate(valueVector)
  end subroutine HandleDuplicates
  !***************************************************
  ! get:
  !     Gives sparse matrix's value from row i
  !     and col j
  !  
  ! Parameters:
  !     Input, i(int), j(int)
  !     Output, get(i,j)(realdp)
  !***************************************************
  real(dp) function get(this, i, j)
    implicit none
    class(Sparse), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: j
    integer :: k
    k = this%AI(i)
    do while(k < this%AI(i+1))
       if(this%AJ(k) == j) then
          get = this%A(k)
          return
       end if
       k = k + 1
    end do
    get = 0.d0
  end function get
  !***************************************************
  ! printValue:
  !     prints a single value either on console or a
  !     given filename
  !  
  ! Parameters:
  !     Input, i(int), j(int), filename(char)[opt]
  !     Output, value printed
  !***************************************************
  subroutine printValue(this, i, j, filename)
    implicit none
    integer, parameter :: fileunit = 90
    class(Sparse), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: j
    character(*), intent(in), optional :: filename
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       write(fileunit,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', j, ' is ', this%get(i,j)
    close(fileunit)
    else
       write(*,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', j, ' is ', this%get(i,j)
    end if
  end subroutine printValue
  !***************************************************
  ! printNonZeros:
  !     Prints all non zeros in a list like format,
  !     either on console or on a given filename
  !  
  ! Parameters:
  !     Input, filename(char)[opt]
  !     Output, non zeros printed
  !***************************************************
  subroutine printNonZeros(this, filename)
    implicit none
    integer, parameter :: fileunit = 91
    class(Sparse), intent(inout) :: this
    character(*), intent(in), optional :: filename
    integer :: i, j
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       do i = 1, this%n
          do j = this%AI(i), this%AI(i+1)-1
             write(fileunit,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', this%AJ(j), ' is ', this%A(j)
          end do
       end do
       close(fileunit)
       return
    end if
    do i = 1, size(this%AI)-1
       do j = this%AI(i), this%AI(i+1)-1
          write(*,'(A,I0,A,I0,A,E14.7)') 'Matriz value at row ', i, ' column ', this%AJ(j), ' is ', this%A(j)
       end do
    end do
  end subroutine printNonZeros
  !***************************************************
  ! printAll:
  !     Prints the whole matrix, zeros included, on
  !     console or on a filename if given.
  !  
  ! Parameters:
  !     Input, filename(char)[opt]
  !     Output, whole matrix printed
  !***************************************************
  subroutine printAll(this, filename)
    implicit none
    integer, parameter :: fileunit = 92
    class(Sparse), intent(inout) :: this
    character(*), intent(in), optional :: filename
    integer :: i, j
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       write(fileunit, '(/,A,I0,A,I0)') 'Printing Sparse Matrix, size: ', size(this%AI)-1, 'x', size(this%AI)-1 
       do i = 1, this%n
          write(fileunit,'(*(E10.4,2X))') (this%get(i,j),j=1,this%n)
       end do
       close(fileunit)
       return
    end if
    write(*, '(A,I0,A,I0)') 'Printing Sparse Matrix, size: ', this%n, 'x', this%n
    do i = 1, this%n
       write(*,'(*(E10.4,2X))') (this%get(i,j),j=1,this%n)
    end do
  end subroutine printAll
  !***************************************************
  ! deleteRowAndCol:
  !     Deletes a given row and column.
  !  
  ! Parameters:
  !     Input, row(int), col(int)
  !     Output, -
  !***************************************************
  subroutine deleteRowAndCol(this, row, col)
    Implicit none
    class(Sparse), intent(inout) :: this
    integer, intent(in) :: row
    integer, intent(in) :: col
    integer :: i, j, k
    integer :: rowSize
    integer, dimension(:), allocatable :: AI

    allocate(AI(size(this%AI)))
    do i = 1, size(AI)
       AI(i) = this%AI(i)
    end do
    deallocate(this%AI)

    if(isCRSDone) then
       do i = size(AI)-1, 1, -1
          if(i == row) then
             rowSize = AI(i+1)-AI(i)
             k = AI(i)
             do while(k < this%nnz-rowSize+1)
                this%AJ(k) = this%AJ(k+rowSize)
                this%A(k) = this%A(k+rowSize)
                k = k + 1
             end do
             this%nnz = this%nnz - rowSize
             do k = row, size(AI)-1
                AI(k) = AI(k+1) - rowSize
             end do
          else 
             do j = AI(i), AI(i+1)-1
                if(this%AJ(j) == col) then
                   k = j
                   do while(k < this%nnz)
                      this%A(k) = this%A(k+1)
                      this%AJ(k) = this%AJ(k+1)
                      k = k + 1
                   end do
                   do k = i+1, size(AI)
                      AI(k) = AI(k)-1
                   end do
                   this%nnz = this%nnz - 1
                end if
             end do
          end if
       end do
       do i = 1, this%nnz
          if(this%AJ(i) > col) then
             this%AJ(i) = this%AJ(i)-1
          end if
       end do
       allocate(this%AI(size(AI)-1))
       do i = 1, size(this%AI)
          this%AI(i) = AI(i)
       end do
    else

    end if
    this%n = this%n - 1
  end subroutine deleteRowAndCol
  !***************************************************
  ! delete:
  !     Clears memory space taken by the sparse matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, -
  !***************************************************
  subroutine delete(this)
    implicit none
    class(Sparse), intent(inout) :: this
    if(allocated(this%A)) deallocate(this%A)
    if(allocated(this%AJ)) deallocate(this%AJ)
    if(allocated(this%AI)) deallocate(this%AI)
  end subroutine delete



  !***********************************************
  !*              MODULE PROCEDURES              *
  !***********************************************
  
  
  !***************************************************
  ! sparse_sparse_prod:
  !     performs the product between two given sparse
  !     matrices. Operator: (*).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_prod(a, b) result(c)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse), intent(in) :: b
    type(Sparse) :: c
    real(dp) :: Cij
    integer :: i, j, k
    integer :: counter, aRowSize, bRowSize, ptr, l
    if(a%n /= b%n) then
       print'(A)', '** diferent sizes in input sparse matrices! **'
       return
    end if
    c = sparse(nnz = a%n*3, rows = a%n)
    counter = 1
    do i = 1, a%n
       aRowSize = a%AI(i+1) - a%AI(i)
       do j = 1, a%n
          Cij = 0
          do k = counter, counter+aRowSize-1
             ptr = b%AI(a%AJ(k))
             bRowSize = b%AI(a%AJ(k)+1) - b%AI(a%AJ(k))
             do l = ptr, ptr+bRowSize-1
                if(j == b%AJ(l)) then
                   Cij = Cij + a%A(k)*b%A(l)
                   EXIT
                end if
             end do
          end do
          if(abs(Cij) > 1d-12) call c%append(Cij, i, j)
       end do
       counter = counter + aRowSize
    end do
    
    call c%makeCRS
  end function sparse_sparse_prod
  !***************************************************
  ! sparse_vect_prod(*):
  !      Performs the product between a sparse matrix
  !      and a condensed real vector.
  !      Operator: (*).
  !  
  ! Parameters:
  !     Input, mat(Sparse), vect(realdp)
  !     Output, res(realdp)
  !***************************************************
  function sparse_vect_prod(mat, vect) result(res)
    implicit none
    type(Sparse), intent(in) :: mat
    real(dp), dimension(:), intent(in) :: vect
    real(dp), dimension(size(vect)) :: res
    real(dp) :: val
    integer :: counter, rowSize, i, k
    res = 0.d0
    counter = 1
    do i = 1, mat%n
       rowSize = mat%AI(i+1) - mat%AI(i)
       val = 0.d0
       do k = counter, counter+rowSize-1
          val = val + mat%A(k)*vect(mat%AJ(k))
       end do
       res(i) = val
       counter = counter + rowSize
    end do
  end function sparse_vect_prod
  !***************************************************
  ! sparse_sparse_add:
  !     performs the addition of sparse a plus
  !     sparse b. Operator: (+).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_add(a, b) result(c)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse), intent(in) :: b
    type(Sparse) :: c
    integer :: counter, rowSize, i, k
    c = sparse(nnz = a%nnz+b%nnz, rows = a%n)
    counter = 1
    do i = 1, a%n
       rowSize = a%AI(i+1) - a%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(a%A(k), i, a%AJ(k))
       end do
       counter = counter + rowSize
    end do
    counter = 1
    do i = 1, b%n
       rowSize = b%AI(i+1) - b%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(b%A(k), i, b%AJ(k))
       end do
       counter = counter + rowSize
    end do
    call c%makeCRS
  end function sparse_sparse_add
  !***************************************************
  ! transpose:
  !     Obtains the transpose of sparse matrix a
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, b(Sparse)
  !***************************************************
  function transpose(a) result(b)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse) :: b
    integer, dimension(a%n) :: colCounter
    integer :: i, j, k, counter
    b = sparse(nnz = a%nnz, rows = a%n)
    colCounter = 0
    do i = 1, b%nnz
       colCounter(a%AJ(i)) = colCounter(a%AJ(i)) + 1
    end do
    allocate(b%AI(b%n+1))
    allocate(b%A(b%nnz))
    allocate(b%AJ(b%nnz))
    b%AI = 1
    do i = 2, b%n+1
       b%AI(i) = b%AI(i-1) + colCounter(i-1)
    end do
    counter = 1
    colCounter = 0
    do i = 1, b%n
       do k = a%AI(i), a%AI(i+1)-1
          j = a%AJ(k)
          b%AJ(b%AI(j)+colCounter(j)) = i
          b%A(b%AI(j)+colCounter(j)) = a%A(counter)
          colCounter(j) = colCounter(j) + 1
          counter = counter + 1
       end do
    end do
  end function transpose
  !***************************************************
  ! norm:
  !     Computes de frobenius-norm of a sparse matrix
  !     https://en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, norm(Realdp)
  !***************************************************
  real(dp) function norm(a)
    implicit none
    type(Sparse), intent(in) :: a
    integer :: i, j, counter
    norm = 0
    counter = 1
    do while(counter <= a%nnz)
       norm = norm + a%A(counter)*a%A(counter)
       counter = counter + 1
    end do
    norm = sqrt(norm)
  end function norm

  
end module SparseKit
