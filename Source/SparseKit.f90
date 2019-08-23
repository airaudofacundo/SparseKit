!*************************************************************
!           Instituto Universitario Aeronautico
!                Dpto. Mecanica Aeronautica
!*************************************************************
! Filename      : SparseKit.f90
! Version       : 0.9
! Date          : 14-08-2019
! Programmer(s) : F. Airaudo(fairaudo574@alumnos.iua.edu.ar)
!                 M. Zuñiga(mzuniga433@alumnos.iua.edu.ar)
!*************************************************************
! Description (brief):
!                     Module for all algebraic and functional
!                     Operations with Sparse Matrices. That is,
!                     For matrices where the number of nonzeros
!                     highly exceeds the number of zeros, this
!                     tools allow you to declare a derived
!                     data type to work with them. 
!************************************************************* 
! Dependecies:
!             Use tools        - Filename: Utilities.f90
!             Use QuickSortMod - Filename: quicksort.f90
!*************************************************************
! Public procedures:
!              Type(Sparse)
!              Derived type Procedures 
!                   Subroutine init
!                   Subroutine update
!                   Subroutine append
!                   Subroutine makeCRS
!                   Function get
!                   Function getNonZeros
!                   Subroutine printValue
!                   Subroutine printNonZeros
!                   Subroutine printAll
!                   Subroutine deleteRowAndCol
!                   Subroutine free
!              Module Procedures:
!                   Function transpose
!                   Function inverse
!                   Function norm
!                   Function gmres
!                   Function jacobiEigen
!                   Function trace
!                   Function inverseGMRESD
!                   Function id
!                   Subroutine sparse_sparse_prod ->  Operator:
!                   Subroutine coef_sparse_prod   ->    
!                   Subroutine sparse_vect_prod   ->     (*)
!                   Subroutine sparse_sparse_sum  ->  Operator:
!                                                        (+)
!                   Subroutine sparse_sparse_sub  ->  Operator:
!                                                        (-)
!*************************************************************
module SparseKit
  !***********************************************
  !*                 EXTERNS                     *
  !***********************************************
  use tools
  use quickSortMod
  implicit none
  private
  public :: Sparse, operator(*), operator(+), operator(-), transpose&
       , norm, gmres, inverse, jacobiEigen, trace, inverseGMRESD, id&
       , lcholesky, det
  type Triplet
     real(rkind), dimension(:), allocatable :: A
     integer, dimension(:), allocatable :: row
     integer, dimension(:), allocatable :: col
  end type Triplet
  type Sparse
     private
     real(rkind), dimension(:), allocatable :: A
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
     procedure, public :: getnnz
     procedure, public :: getn
     
     procedure, public :: printValue
     procedure, public :: printNonZeros
     procedure, public :: printAll

     procedure, public :: deleteRowAndCol

     procedure, public :: free
     
     procedure, private :: handleDuplicates
  end type Sparse

  interface operator(*)
     module procedure sparse_sparse_prod
     module procedure sparse_vect_prod
     module procedure coef_sparse_prod
  end interface operator(*)
  interface operator(+)
     module procedure sparse_sparse_add
  end interface operator(+)
  interface operator(-)
     module procedure sparse_sparse_sub
  end interface operator(-)
  interface sparse
     procedure constructor
  end interface sparse
  interface transpose
     module procedure transpose
  end interface transpose
  interface norm
     module procedure norm
  end interface norm
  interface trace
     module procedure trace
  end interface trace
  interface det
     module procedure det
  end interface det
  interface lcholesky
     module procedure lcholesky
  end interface lcholesky
  interface gmres
     module procedure gmres 
  end interface gmres
  interface inverse
     module procedure inverse
  end interface inverse
  interface jacobiEigen
     module procedure jacobiEigen
  end interface jacobiEigen
  
  
  !***********************************************
  !*          LOCAL PRIVATE VARIABLES            *
  !***********************************************
  logical :: isCRSDone = .false.
  real(rkind), dimension(:), allocatable :: valueVector
  real(rkind), dimension(:), allocatable :: auxA
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
  !     Input, value(realrkind), row(int), col(int)
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
  subroutine makeCRS(this, sortRows)
    implicit none
    class(Sparse), intent(InOut) :: this
    logical, intent(in), optional :: sortRows
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
    if(.not.present(sortRows) .or. sortRows) then
       call quicksort(this%triplet%row, this%triplet%col, this%triplet%A, 1, this%counter)
    end if
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
                k = k - 1
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
  !     Output, get(i,j)(realrkind)
  !***************************************************
  real(rkind) function get(this, i, j)
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
  ! getnnz:
  !     given ammount of non zeros
  !  
  ! Parameters:
  !     Input, -
  !     Output, getnnz()(integer)
  !***************************************************
  integer function getnnz(this)
    implicit none
    class(Sparse), intent(inout) :: this
    getnnz = this%nnz
  end function getnnz
  !***************************************************
  ! getn:
  !     get order of matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, getn()(integer)
  !***************************************************
  integer function getn(this)
    implicit none
    class(Sparse), intent(inout) :: this
    getn = this%n
  end function getn
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
       write(fileunit,'(A,I0,A)') 'Printing ', this%nnz, ' non zeros'
       write(fileunit,'(A,I0,A,I0)') 'Matrix dimension: ', this%n, 'x', this%n
       write(fileunit,'(A)') '------------------------------------'
       write(fileunit,'(A8,4X,2A12)') 'value', 'row', 'column'
       write(fileunit,'(A)') '------------------------------------'
       do i = 1, this%n
          do j = this%AI(i), this%AI(i+1)-1
             write(fileunit,'(E12.6,I12,I12)')  this%A(j), i, this%AJ(j)
          end do
       end do
       close(fileunit)
       return
    end if
    write(*,'(A,I0,A)') 'Printing ', this%nnz, ' non zeros'
    write(*,'(A,I0,A,I0)') 'Matrix dimension: ', this%n, 'x', this%n
    write(*,'(A)') '------------------------------------'
    write(*,'(A8,4X,2A12)') 'value', 'row', 'column'
    write(*,'(A)') '------------------------------------'
    do i = 1, size(this%AI)-1
       do j = this%AI(i), this%AI(i+1)-1
             write(*,'(E12.6,I12,I12)')  this%A(j), i, this%AJ(j)
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
    if(isCRSDone) then
       allocate(AI(size(this%AI)))
       do i = 1, size(AI)
          AI(i) = this%AI(i)
       end do
       deallocate(this%AI)
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
       i = 1
       do while(i < this%counter)
          if(this%triplet%row(i) == row .or. this%triplet%col(i) == col) then
             this%triplet%row(i) = this%triplet%row(this%counter)
             this%triplet%col(i) = this%triplet%col(this%counter)
             this%triplet%A(i) = this%triplet%A(this%counter)
             this%counter = this%counter - 1
          else
             i = i + 1
          end if
       end do
       if(this%triplet%row(i) == row .or. this%triplet%col(i) == col) then
          this%counter = this%counter - 1
       end if
       do i = 1, this%counter
          if(this%triplet%row(i) > row) then
             this%triplet%row(i) = this%triplet%row(i) - 1
          end if
          if(this%triplet%col(i) > col) then
             this%triplet%col(i) = this%triplet%col(i) - 1
          end if
       end do
    end if
    this%n = this%n - 1
  end subroutine deleteRowAndCol
  !***************************************************
  ! free:
  !     Clears memory space taken by the sparse matrix
  !  
  ! Parameters:
  !     Input, -
  !     Output, -
  !***************************************************
  subroutine free(this)
    implicit none
    class(Sparse), intent(inout) :: this
    if(allocated(this%A)) deallocate(this%A)
    if(allocated(this%AJ)) deallocate(this%AJ)
    if(allocated(this%AI)) deallocate(this%AI)
  end subroutine free



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
    type(Sparse) :: bTranspose
    real(rkind) :: Cij
    integer :: i, j, k
    integer :: nnz
    logical :: nnzFound
    if(a%n /= b%n) then
       print'(A)', '** diferent sizes in input sparse matrices! **'
       return
    end if
    bTranspose = transpose(b)
    !find ammount of nnz
    nnz = 0
    do i = 1, a%n
       do j = 1, bTranspose%n
          nnzFound = .false.
          do k = a%AI(i), a%AI(i+1)-1
             do l = bTranspose%AI(j), bTranspose%AI(j+1)-1
                if(a%AJ(k) == bTranspose%AJ(l)) nnzFound = .true.
             end do
          end do
          if(nnzFound) nnz = nnz + 1
       end do
    end do
    c = sparse(nnz = nnz , rows = a%n)
    do i = 1, a%n
       do j = 1, bTranspose%n
          Cij = 0
          do k = a%AI(i), a%AI(i+1)-1
             do l = bTranspose%AI(j), bTranspose%AI(j+1)-1
                if(a%AJ(k) == bTranspose%AJ(l)) then
                   Cij = Cij + a%A(k)*bTranspose%A(l)
                end if
             end do
          end do
          if(abs(Cij) > 1d-12) call c%append(Cij, i, j)
       end do
    end do
    call bTranspose%free()
    call c%makeCRS(.false.)
  end function sparse_sparse_prod
  !***************************************************
  ! sparse_vect_prod(*):
  !      Performs the product between a sparse matrix
  !      and a condensed real vector.
  !      Operator: (*).
  !  
  ! Parameters:
  !     Input, mat(Sparse), vect(realrkind)
  !     Output, res(realrkind)
  !***************************************************
  function sparse_vect_prod(mat, vect) result(res)
    implicit none
    type(Sparse), intent(in) :: mat
    real(rkind), dimension(:), intent(in) :: vect
    real(rkind), dimension(size(vect)) :: res
    real(rkind) :: val
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
  ! coef_sparse_prod(*):
  !      Performs the product between a sparse matrix
  !      and a coeficient real vector.
  !      Operator: (*).
  !  
  ! Parameters:
  !     Input, mat(Sparse), coef(realrkind)
  !     Output, res(realrkind)
  !***************************************************
  function coef_sparse_prod(coef ,mat) result(res)
    implicit none
    type(Sparse), intent(in) :: mat
    type(Sparse) :: res
    real(rkind), intent(in) :: coef
    real(rkind) :: c
    integer :: i
    c = coef
    res = mat
    do i = 1, res%nnz
       res%A(i) = res%A(i)*c
    end do
  end function coef_sparse_prod
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
  ! sparse_sparse_sub:
  !     performs the subtraction of sparse a and
  !     sparse b. Operator: (-).
  !  
  ! Parameters:
  !     Input, a(Sparse), b(Sparse)
  !     Output, c(Sparse)
  !***************************************************
  function sparse_sparse_sub(a, b) result(c)
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
          call c%append(-1.d0*b%A(k), i, b%AJ(k))
       end do
       counter = counter + rowSize
    end do
    call c%makeCRS
  end function sparse_sparse_sub
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
  !     Computes the frobenius-norm of a sparse matrix
  !     https://en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, norm(Realrkind)
  !***************************************************
  real(rkind) function norm(a)
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
  !***************************************************
  ! trace:
  !     Computes the sum of the elements of the
  !     diagonal of a sparse matrix
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, trace(Realrkind)
  !***************************************************
  real(rkind) function trace(a)
    implicit none
    type(Sparse), intent(in) :: a
    integer :: i, j
    trace = 0
    do i = 1, a%n
       do j = a%AI(i), a%AI(i+1)-1
          if(a%AJ(j) == i) then
             trace = trace + a%A(j)
             exit
          end if
       end do
    end do
  end function trace
  !***************************************************
  ! det:
  !     Computes the determinant
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, det(Realrkind)
  !***************************************************
  real(rkind) function det(a)
    implicit none
    type(Sparse), intent(in) :: a
    type(Sparse) :: m
    integer :: i, j
    det = 1.
    m = lcholesky(a)
    do i = 1, m%n
       do j = m%AI(i), m%AI(i+1)-1
          if(m%AJ(j) == i) then
             det = det * m%A(j)
             exit
          end if
       end do
    end do
    det = det**2
  end function det
  !***************************************************
  ! id:
  !     Computes the identity matrix of order n
  !  
  ! Parameters:
  !     Input, n(integer)
  !     Output, id(Sparse)
  !***************************************************
  function id(n) result(a)
    implicit none
    type(Sparse) :: a
    integer, intent (in) :: n
    integer :: i
    a = sparse(nnz = n, rows = n)
    do i = 1, n
       call a%append(1, i, i)
    end do
    call a%makeCRS
  end function id
  !***************************************************
  ! lcholesky:
  !     Computes Incomplete Cholesky factorization
  !  
  ! Parameters:
  !     Input, a(Sparse)
  !     Output, L(Sparse)
  !***************************************************
  function lcholesky(a) result(L)
    implicit none
    type(Sparse) :: a
    type(Sparse) :: L
    integer :: i, j, k
    real(rkind) :: adder1, adder2, m( a%n, a%n)
    L = sparse(nnz = (a%n**2+a%n)/2, rows = a%n)
    m = 0.
    do i = 1, a%n
       do j = 1, a%n
          adder1 = 0.
          do k = 1, j-1
             adder1 = adder1 + m(j,k)**2
          end do
          m(j,j) = sqrt(a%get(j,j)-adder1)
          if( i > j) then
             adder2 = 0.
             do k = 1 , j-1
                adder2 = adder2 + ( m(i,k) * m(j,k))
             end do
             m(i,j) = (1./m(j,j))*(a%get(i,j)-adder2)
          end if
       end do
    end do
    do i = 1, a%n
          call L%append( m(i,i), i, i)
       do j = 1, a%n
          if( i > j) then
             call L%append( m(i,j), i, j)
          end if
       end do
    end do
    call L%makeCRS
    return
  end function lcholesky
  !***************************************************
  ! gmres: 
  !   (generalized minimal residual method)
  !   Approximates the solution of a nonsymmetric system 
  !   of linear equations by the vector in a Krylov 
  !   subspace with minimal residual.
  !  
  ! Parameters:
  !     Input, A(Sparse), rhs(realrkind)
  !     Output, x(realrkind)
  !***************************************************
  function gmres(A, rhs) result(x)
    implicit none
    Type(Sparse), intent(in) :: A
    Real(rkind), intent(in)  :: rhs(A%n)
    Real(rkind) :: x(A%n)
    Integer, parameter :: itr_max = 1000
    Real(rkind), parameter :: tol_abs = 1d-15
    Real(rkind), parameter :: delta = 1.0D-03
    Real(rkind), parameter :: tol_rel = 1d-15
    Real(rkind) :: av, c(A%n), g(A%n), h(A%n,A%n-1)
    Real(rkind) :: htmp, tl, l(A%AI(A%n+1)+1), mu
    Real(rkind) :: g1, g2, r(A%n), rho, rho_tol
    Real(rkind) :: s(A%n), v(A%n,A%n), y(A%n), w(A%n)
    Integer :: i, j, k, itr, itr_used
    Integer :: iw(A%n), jj, jrow, jw, k_copy, ua(A%n)
    itr_used = 0
    ua(1:A%n) = -1
    !  Finds diagonal entries
    do i = 1, A%n
       do k = A%AI(i), A%AI(i+1) - 1
          if ( A%AJ(k) == i ) then
             ua(i) = k
          end if
       end do
    end do   
    !  Obtains the unit lower triangular
    !  Copy A.
    l(1:A%nnz) = A%A(1:A%nnz)
    do i = 1, A%n
    !  IW points to the nonzero entries in row I.
       iw(1:A%n) = -1       
       do k = A%AI(i), A%AI(i+1) - 1
          iw(A%AJ(k)) = k
       end do      
       do j = A%AI(i), A%AI(i+1) - 1
          jrow = A%AJ(j)
          if ( i <= jrow ) then
             exit
          end if
          tl = l(j) * l(ua(jrow))
          l(j) = tl
          do jj = ua(jrow) + 1, A%AI(jrow+1) - 1
             jw = iw(A%AJ(jj))
             if ( jw /= -1 ) then
                l(jw) = l(jw) - tl * l(jj)
             end if
          end do
       end do
       ua(i) = j
       l(j) = 1. / l(j)
    end do
    l(ua(1:A%n)) = 1. / l(ua(1:A%n))    
    do itr = 1, itr_max
       r = A*x
       r(1:A%n) = rhs(1:A%n) - r(1:A%n)
    !  Solve l*u*r=r
    !  Copy r in.
    w(1:A%n) = r(1:A%n)
    !  Solve L * w = w where L is unit lower triangular.
    do i = 2, A%n
       do j = A%AI(i), ua(i) - 1
          w(i) = w(i) - l(j) * w(A%AJ(j))
       end do
    end do
    !  Solve U * w = w, where U is upper triangular.
    do i = A%n, 1, -1
       do j = ua(i) + 1, A%AI(i+1) - 1
          w(i) = w(i) - l(j) * w(A%AJ(j))
       end do
       w(i) = w(i) / l(ua(i))
    end do
    !  Copy Z out.
    r(1:A%n) = w(1:A%n)
       rho = sqrt ( dot_product ( r, r ) )
       if ( itr == 1 ) then
          rho_tol = rho * tol_rel
       end if
       v(1:A%n,1) = r(1:A%n) / rho
       g(1) = rho
       g(2:A%n) = 0.
       h(1:A%n,1:A%n-1) = 0.
       do k = 1, A%n-1
          k_copy = k
          v(1:A%n,k+1) = A* v(1:A%n,k)        
    !  Solve l*u*r=r
    !  Copy r in.
    w(1:A%n) = v(1:A%n,k+1)
    !  Solve L * w = w where L is unit lower triangular.
    do i = 2, A%n
       do j = A%AI(i), ua(i) - 1
          w(i) = w(i) - l(j) * w(A%AJ(j))
       end do
    end do
    !  Solve U * w = w, where U is upper triangular.
    do i = A%n, 1, -1
       do j = ua(i) + 1, A%AI(i+1) - 1
          w(i) = w(i) - l(j) * w(A%AJ(j))
       end do
       w(i) = w(i) / l(ua(i))
    end do
    !  Copy Z out.
    v(1:A%n,k+1) = w(1:A%n)
          av = sqrt ( dot_product ( v(1:A%n,k+1), v(1:A%n,k+1) ) )
          do j = 1, k
             h(j,k) = dot_product ( v(1:A%n,k+1), v(1:A%n,j) )
             v(1:A%n,k+1) = v(1:A%n,k+1) - v(1:A%n,j) * h(j,k)
          end do
          h(k+1,k) = sqrt ( dot_product ( v(1:A%n,k+1), v(1:A%n,k+1) ) )
          if ( ( av + delta * h(k+1,k)) == av ) then
             do j = 1, k
                htmp = dot_product ( v(1:A%n,k+1), v(1:A%n,j) )
                h(j,k) = h(j,k) + htmp
                v(1:A%n,k+1) = v(1:A%n,k+1) - htmp * v(1:A%n,j)
             end do
             h(k+1,k) = sqrt ( dot_product ( v(1:A%n,k+1), v(1:A%n,k+1) ) )
          end if
          if ( h(k+1,k) /= 0. ) then
             v(1:A%n,k+1) = v(1:A%n,k+1) / h(k+1,k)
          end if
          if ( 1 < k ) then
             y(1:k+1) = h(1:k+1,k)
             do j = 1, k - 1
                g1 = c(j) * y(j) - s(j) * y(j+1)
                g2 = s(j) * y(j) + c(j) * y(j+1)
                y(j)   = g1
                y(j+1) = g2                
             end do
             h(1:k+1,k) = y(1:k+1)
          end if
          mu = sqrt ( h(k,k)**2 + h(k+1,k)**2 )
          c(k) = h(k,k) / mu
          s(k) = -h(k+1,k) / mu
          h(k,k) = c(k) * h(k,k) - s(k) * h(k+1,k)
          h(k+1,k) = 0.
          g1 = c(k) * g(k) - s(k) * g(k+1)
          g2 = s(k) * g(k) + c(k) * g(k+1)          
          g(k)   = g1
          g(k+1) = g2
          rho = abs ( g(k+1) )
          itr_used = itr_used + 1
          if ( rho <= rho_tol .and. rho <= tol_abs ) then
             exit
          end if
       end do
       k = k_copy - 1
       y(k+1) = g(k+1) / h(k+1,k+1)
       do i = k, 1, -1
          y(i) = ( g(i) - dot_product ( h(i,i+1:k+1), y(i+1:k+1) ) ) / h(i,i)
       end do
       do i = 1, A%n
          x(i) = x(i) + dot_product ( v(i,1:k+1), y(1:k+1) )
       end do
       if ( rho <= rho_tol .and. rho <= tol_abs ) then
          exit
       end if
    end do
    return
  end function gmres  
  !***************************************************
  ! Inverse:
  !    Obtains the inverse of sparse matrix A
  !  
  ! Parameters:
  !     Input, A(Sparse)
  !     Output, B(Sparse)
  !***************************************************
  function inverse(A) result(B)
    implicit none
    type(Sparse), intent(in) :: A
    type(Sparse) :: B
    real(rkind) :: y(A%n), x(A%n)
    integer :: i, j
    B = sparse(nnz = A%n**2, rows = A%n)
    do j = 1,A%n
       y = 0.
       y(j) = 1.
       x = gmres(A,y)
       do i = 1, A%n
          if(abs(x(i)).gt.1d-5)then
             call B%append(x(i), i, j)
          end if
       end do
    end do
    call B%makeCRS
    return
  end function inverse
    !***************************************************
  ! inverseGMRESD:
  !    Obtains the inverse of sparse matrix A
  !    (Global Minimal Residual descent algorithm)
  ! Parameters:
  !     Input, A(Sparse)
  !     Output, B(Sparse)
  !***************************************************
  function inverseGMRESD(A) result(M)
    implicit none
    type(Sparse), intent(in) :: A
    type(Sparse) :: B
    type(Sparse) :: C
    type(Sparse) :: G
    type(Sparse) :: M
    real(rkind) :: alpha
    alpha = 1.
    M = transpose(A)
    do while (abs(alpha) > 1e-30)
       C = A * M
       G = Id(A%n) - C
       B = A * G
       alpha = trace(transpose(G)*B)/(norm(B))**2
       M = M + alpha * G
    end do
    return
  end function inverseGMRESD
  !***************************************************
  ! jacobiEigen:
  !    Obtains the eigenvalues and eigenvectors of a
  !    real symmetric matrix A, using Rutishauser's
  !    modfications of the classical Jacobi rotation
  !    method with threshold pivoting.
  !
  ! Parameters:
  !     Input, A(Sparse)
  !     Output, Eigenvec(realrkind), the matrix of eigenvectors
  !     Output, Eigenval(realrkind), the eigenvalues.
  !***************************************************
  subroutine jacobiEigen(A_input, Eigenval, Eigenvec) 
    implicit none
    type(Sparse), intent(in) :: A_input
    real(rkind), intent(out) :: Eigenval(A_input%n),Eigenvec(A_input%n,A_input%n)
    integer, parameter :: it_max = 1000
    integer :: i, j, k, l, m, p, q, it_num, rot_num
    real(rkind) :: a(A_input%n,A_input%n), bw(A_input%n),  w(A_input%n), zw(A_input%n) 
    real(rkind) :: h, s, t, tau, term, termp, termq, theta, c, g, thresh, gapq
    a = 0.
    Do i = 1, A_input%n
       Do j = A_input%AI(i), A_input%AI(i+1)-1
          a(i, A_input%AJ(j))  = A_input%A(j)
       End Do
    End Do
    Eigenvec = 0.
    do i = 1, A_input%n
       Eigenvec(i,i) = 1.
    end do
    do i = 1, A_input%n
       Eigenval(i) = a(i,i)
    end do
    bw(1:A_input%n) = Eigenval(1:A_input%n)
    zw(1:A_input%n) = 0.
    it_num = 0
    rot_num = 0
    do while ( it_num < it_max )
       it_num = it_num + 1
       !  The convergence threshold is based on the size of the elements in
       !  the strict upper triangle of the matrix.
       thresh = 0.
       do j = 1, A_input%n
          do i = 1, j - 1
             thresh = thresh + a(i,j) ** 2
          end do
       end do
       thresh = sqrt ( thresh ) / (4.*A_input%n)
       if ( thresh == 0. ) then
          exit 
       end if
       do p = 1, A_input%n
          do q = p + 1, A_input%n
             gapq = 10. * abs ( a(p,q) )
             termp = gapq + abs ( Eigenval(p) )
             termq = gapq + abs ( Eigenval(q) )
             !  Annihilate tiny offdiagonal elements.
             if ( 4 < it_num .and. &
                  termp == abs ( Eigenval(p) ) .and. &
                  termq == abs ( Eigenval(q) ) ) then
                a(p,q) = 0.
                !  Otherwise, apply a rotation.
             else if ( thresh <= abs ( a(p,q) ) ) then
                h = Eigenval(q) - Eigenval(p)
                term = abs ( h ) + gapq
                if ( term == abs ( h ) ) then
                   t = a(p,q) / h
                else
                   theta = 0.5 * h / a(p,q)
                   t = 1. / ( abs ( theta ) + sqrt ( 1. + theta * theta ) )
                   if ( theta < 0. ) then 
                      t = - t
                   end if
                end if
                c = 1. / sqrt ( 1. + t * t )
                s = t * c
                tau = s / ( 1. + c )
                h = t * a(p,q)
                !  Accumulate corrections to diagonal elements.
                zw(p) = zw(p) - h                  
                zw(q) = zw(q) + h
                Eigenval(p) = Eigenval(p) - h
                Eigenval(q) = Eigenval(q) + h
                a(p,q) = 0.
                !  Rotate, using information from the upper triangle of A only.
                do j = 1, p - 1
                   g = a(j,p)
                   h = a(j,q)
                   a(j,p) = g - s * ( h + g * tau )
                   a(j,q) = h + s * ( g - h * tau )
                end do
                do j = p + 1, q - 1
                   g = a(p,j)
                   h = a(j,q)
                   a(p,j) = g - s * ( h + g * tau )
                   a(j,q) = h + s * ( g - h * tau )
                end do
                do j = q + 1, A_input%n
                   g = a(p,j)
                   h = a(q,j)
                   a(p,j) = g - s * ( h + g * tau )
                   a(q,j) = h + s * ( g - h * tau )
                end do
                !  Accumulate information in the eigenvector matrix.
                do j = 1, A_input%n
                   g = Eigenvec(j,p)
                   h = Eigenvec(j,q)
                   Eigenvec(j,p) = g - s * ( h + g * tau )
                   Eigenvec(j,q) = h + s * ( g - h * tau )
                end do
                rot_num = rot_num + 1
             end if
          end do
       end do
       bw(1:A_input%n) = bw(1:A_input%n) + zw(1:A_input%n)
       Eigenval(1:A_input%n) = bw(1:A_input%n)
       zw(1:A_input%n) = 0.
    end do
    !  Restore upper triangle of input matrix.
    do j = 1, A_input%n
       do i = 1, j - 1
          a(i,j) = a(j,i)
       end do
    end do
    !  Ascending sort the eigenvalues and eigenvectors.
    do k = 1, A_input%n - 1
       m = k
       do l = k + 1, A_input%n
          if ( Eigenval(l) < Eigenval(m) ) then
             m = l
          end if
       end do
       if ( m /= k ) then
          t    = Eigenval(m)
          Eigenval(m) = Eigenval(k)
          Eigenval(k) = t
          w(1:A_input%n)   = Eigenvec(1:A_input%n,m)
          Eigenvec(1:A_input%n,m) = Eigenvec(1:A_input%n,k)
          Eigenvec(1:A_input%n,k) = w(1:A_input%n)
       end if
    end do
  end subroutine jacobiEigen
end module SparseKit
