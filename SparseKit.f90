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
  public :: Sparse, operator(*), operator(+)
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

     procedure, public :: deleteRow
     
     procedure, private :: handleDuplicates
  end type Sparse

  interface operator(*)
     module procedure sparse_sparse_prod
     module procedure sparse_vect_prod
  end interface operator(*)
  interface operator(+)
     module procedure sparse_sparse_add
  end interface operator(+)
  
  !***********************************************
  !*          LOCAL PRIVATE VARIABLES            *
  !***********************************************
  logical :: isCRSDone = .false.
  real(dp), dimension(:), allocatable :: valueVector
  real(dp), dimension(:), allocatable :: auxA
  integer, dimension(:), allocatable :: auxAJ
  integer, dimension(:), allocatable :: rowVector
  integer :: repeats, i, j, k, l, n, m

  
  interface sparse
     procedure constructor
  end interface sparse
  
contains
  !***************************************************
  ! Constructor:
  !     Initializes the object with and estimate of
  !     the matrix's non zeros and the number of rows
  !  
  ! Parameters:
  !     Input, nnz, rows
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
    n = nnz
    m = rows
    allocate(this%triplet%A(n))
    allocate(this%triplet%row(n))
    allocate(this%triplet%col(n))
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
  !     Input, nnz, rows
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
    n = nnz
    m = rows
    allocate(this%triplet%A(n))
    allocate(this%triplet%row(n))
    allocate(this%triplet%col(n))
    this%triplet%A = 0
    this%triplet%row = 0
    this%triplet%col = 0
    this%counter = 0
  end subroutine update
  !***************************************************
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  subroutine makeCRS(this)
    implicit none
    class(Sparse), intent(InOut) :: this
    isCRSDone = .true.
    allocate(this%rowCounter(m))
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
    allocate(this%AI(m+1))
    this%AI = 1
    do i = 2, m+1
       this%AI(i) = this%AI(i-1) + this%rowCounter(i-1)
    end do
    deallocate(this%triplet%A, this%triplet%row, this%triplet%col)
    deallocate(auxA, auxAJ)
  end subroutine makeCRS
  !***************************************************
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  subroutine handleDuplicates(this)
    implicit none
    class(Sparse), intent(InOut) :: this
    logical :: mask
    allocate(rowVector(maxval(this%rowCounter)))
    allocate(valueVector(maxval(this%rowCounter)))
    this%counter = 1
    repeats = 0
    do i = 1, m
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  real(dp) function get(this, i, j)
    implicit none
    class(Sparse), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: j
    k = this%AI(i)
    do while(k <= this%AI(i+1))
       if(this%AJ(k) == j) then
          get = this%A(k)
          return
       end if
       k = k + 1
    end do
    get = 0.d0
  end function get
  !***************************************************
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  subroutine printNonZeros(this, filename)
    implicit none
    integer, parameter :: fileunit = 91
    class(Sparse), intent(inout) :: this
    character(*), intent(in), optional :: filename
    integer :: i, j
    if(present(filename)) then
       open(fileunit, file = trim(filename), access = 'append')
       do i = 1, size(this%AI)-1
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
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
       do i = 1, size(this%AI)-1
          write(fileunit,'(*(E10.4,2X))') (this%get(i,j),j=1,size(this%AI)-1)
       end do
       close(fileunit)
       return
    end if
    write(*, '(A,I0,A,I0)') 'Printing Sparse Matrix, size: ', size(this%AI)-1, 'x', size(this%AI)-1 
    do i = 1, size(this%AI)-1
       write(*,'(*(E10.4,2X))') (this%get(i,j),j=1,size(this%AI)-1)
    end do
  end subroutine printAll
  !***************************************************
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  subroutine deleteRow(this, row) !Capaz lo junto con el delete col?
    Implicit none
    class(Sparse), intent(inout) :: this
    integer, intent(in) :: row
    integer :: i, k, rowSize
    integer, dimension(:), allocatable :: AI
    
    if(isCRSDone) then
       rowSize = this%AI(row+1)-this%AI(row)
       k = this%AI(row)
       do while(k < size(this%AJ)-rowSize)
          this%AJ(k) = this%AJ(k+rowSize)
          this%A(k) = this%A(k+rowSize)
          k = k + 1
       end do
       allocate(AI(size(this%AI)-1))
       do i = 1, row-1
          AI(i) = this%AI(i)
       end do
       do i = row, size(AI)
          AI(i) = this%AI(i+1)-rowSize
       end do
       deallocate(this%AI)
       allocate(this%AI(size(AI)))
       do i = 1, size(AI)
          this%AI(i) = AI(i)
       end do
       deallocate(AI)
    else
       
    end if
  end subroutine deleteRow
  !***************************************************
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  function sparse_sparse_prod(a, b) result(c)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse), intent(in) :: b
    type(Sparse) :: c
    real(dp) :: Cij
    integer :: counter, aRowSize, bRowSize, ptr, l
    if(size(a%AI) /= size(b%AI)) then
       print'(A)', '** diferent sizes in input sparse matrices! **'
       return
    end if
    c = sparse(nnz = size(a%AJ)*3, rows = size(a%AI)-1)
    counter = 1
    do i = 1, size(a%AI)-1
       aRowSize = a%AI(i+1) - a%AI(i)
       do j = 1, size(a%AI)-1
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  function sparse_vect_prod(mat, vect) result(res)
    implicit none
    type(Sparse), intent(in) :: mat
    real(dp), dimension(:), intent(in) :: vect
    real(dp), dimension(size(vect)) :: res
    real(dp) :: val
    integer :: counter, rowSize, i
    res = 0.d0
    counter = 1
    do i = 1, size(mat%AI)-1
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
  ! RutinaNombre:
  !     Descripcion asd asd as d 
  !  
  ! Parameters:
  !     Input, ...
  !     Output, ...
  !***************************************************
  function sparse_sparse_add(a, b) result(c)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse), intent(in) :: b
    type(Sparse) :: c
    integer :: counter, rowSize, i, k
    c = sparse(nnz = size(a%AJ)+size(b%AJ), rows = size(a%AI)-1)
    counter = 1
    do i = 1, size(a%AI)-1
       rowSize = a%AI(i+1) - a%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(a%A(k), i, a%AJ(k))
       end do
       counter = counter + rowSize
    end do
    counter = 1
    do i = 1, size(b%AI)-1
       rowSize = b%AI(i+1) - b%AI(i)
       do k = counter, counter+rowSize-1
          call c%append(b%A(k), i, b%AJ(k))
       end do
       counter = counter + rowSize
    end do
    call c%makeCRS
  end function sparse_sparse_add
  


  
end module SparseKit
