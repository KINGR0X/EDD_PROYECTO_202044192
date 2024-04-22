# Manual tecnico

## Requisitos del sistema

### Sistemas operativos compatibles

- Windows 10,11

### Memporia Ram

- Minimo 4gb de Ram

### Lenguaje de programación

- Fortran

## Descripción general

Se desarrolla una aplicación en consola que pueda ejecutarse independientemente del
sistema operativo de la pc, por lo que se propone crear una aplicación de consola en el
lenguaje de programación Fortran, esta aplicación debe permitir a los clientes de la
empresa Pixel Print Studio registrar imágenes especiales construidas por capas. Para poder
hacer uso de la aplicación el cliente debe registrarse.
La principal funcionalidad de la aplicación consiste en un generador de imágenes
por capas, la aplicación contará con un conjunto de capas cargadas previamente y
almacenadas en memoria para ser utilizadas, estas capas se utilizarán para generar
imágenes hechas con pixeles, cada capa contendrá la información de los distintos
píxeles y al colocar una capa sobre otra estas irán formando una imagen más
completa.
El sistema es capaz de generar imagen seleccionando las capas deseadas.

## Archivo main.f90

Este es elarchivo principal, donde se hace uso de las diferentes estructuras de datos para procesar y alamcenar la información.

extracto delas variables utilzadas

```fortran
program main
    use json_module
    ! use matrix_m
    use abb_m
    use lista_album
    use clientes_f2
    use avl_m

    use matriz_dis

    implicit none

    ! Definir las variables admin y password
    character(len=10) :: admin
    character(len=10) :: password

    ! Usuario y clave para el inicio de sesión
    character(len=10) :: usuario
    character(len=10) :: clave

    ! Lo que se va a usar para leer el Json
    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, id_capa_pointer, pixelsPointer, imgs_album_Pointer, attributePointer, pixelAttribute  ! punteros
    type(json_value), pointer :: nombre_c_pointer,password_c_pointer
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    integer:: id_capa, colum, row
```

Desde este archivo es donde se muestran por consola los menus, y se procesan los archivos json.

```fortran
    subroutine mostrarMenu()
        print *, " "
        print *, "============ Menu ============"
        print *, "Seleccione una opcion:"
        print *, "1. Inicio de sesion"
        print *, "2. Registro de usuarios"
        print *, "3. Salir"
        print *, "=============================="
    end subroutine mostrarMenu

    subroutine mostrarMenuCliente()
        implicit none
        integer :: opcionCliente
        do
            print *, " "
            print *, "============ Menu Cliente ============"
            print *, "Seleccione una opcion:"
            print *, "1. Visualizar reportes de las estructuras"
            print *, "2. Generacion de imagen por capa"
            print *, "3. Generacion de imagen en Preorden"
            print *, "4. Generacion de imagen en Inorden"
            print *, "5. Generacion de imagen en Postorden"

            print *, "6. Carga masiva de capas"
            print *, "7. Carga masiva de imagenes"
            print *, "8. Cargar masiva de albumes"

            print *, "9. Reporte: ListarCapas (preorder,inorder y postorder)"
            print *, "10. Salir"
            print *, "=============================="
            read(*, *) opcionCliente
            select case(opcionCliente)
                case(1)
                    print *, "Visualizando reportes de las estructuras..."

                    call abrir_grafica("arbol_b_capas.png")
                    call abrir_grafica("avl_tree.png")
                    call abrir_grafica("lista_album.dot.png")

                case(2)
                    print *, "Generando imagen por capa..."
                    call img_por_capa()

                    call abrir_grafica("capas_prueba.dot.png")

                case(3)
                    print *, "Generando imagen en Preorden..."
                    call img_preorden()

                    call abrir_grafica("preorden.dot.png")
                case(4)
                    print *, "Generando imagen en Inorden..."

                    call img_inorden()

                    call abrir_grafica("inorden.dot.png")
                case(5)
                    print *, "Generando imagen en Postorden..."

                    call img_postorden()

                    call abrir_grafica("postorden.dot.png")
                case(6)
                    print *, "Cargando masivamente capas..."

                    call cargarCapas()

                    call abrir_grafica("arbol_b_capas.png")

                case(7)
                    print *, "Cargando masivamente imágenes..."

                    call cargar_imgs()

                    call abrir_grafica("avl_tree.png")
                case(8)
                    print *, "Cargando masivamente albumes..."
                    call cargar_albumes()

                    call abrir_grafica("lista_album.dot.png")

                case(9)
                    print *, "Listando capas en preorden, inorden y postorden..."

                    call reporte()
                case(10)
                    exit
                case default
                    print *, "Opción inválida, por favor seleccione nuevamente."
            end select
        end do
    end subroutine mostrarMenuCliente
```

## abb.f90

En este archivo es donde se encuentra la estructura de datos del arbol binario de busqueda, y las funciones para insertar, eliminar, buscar nodos, y recorrer el arbol en preorden, inorden y postorden.

```fortran

module abb_m
use uuid_module
implicit none
private

    type :: node
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: abb
        type(node), pointer :: root => null()
    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: preorder_get
        procedure :: inorder_get
        procedure :: postorder_get
    end type abb

contains

```

## lista_album.f90

En este archivo es donde se encuentra la estructura de datos de la lista doblemente enlazada, cada nodo de la lista doblemente enlazada contieneuna pila.Se encuentran las funciones para insertar, eliminar, buscar nodos, graficar.

```fortran
module lista_album
    implicit none
    private

    type :: sub_node
        integer:: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type, public :: node
        private
        integer :: value
        character(:), allocatable :: name_cliente
        type(sub_node), pointer :: stack => null() ! Pila asociada al nodo
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()

    contains
        procedure :: append_to_stack

    end type node

    ! Lista circular
    type, public :: lista_circular
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: append_img
        procedure :: get_size
        procedure :: graficar
        procedure :: get_stack_size
        procedure :: print_pointers
    end type lista_circular

contains

```

## avl.f90

Como su nombre indica es el archivo donde se guarda la estructura de datos del arbol avl, y las funciones para insertar, eliminar, buscar nodos, y recorrer el arbol en preorden, inorden y postorden.

```f
module avl_m
    use abb_m
    use uuid_module
    implicit none
    private

    type :: node
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
        type(abb), pointer :: abb_tree => null()


    end type node

    type, public :: avl
        type(node), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: graph
        procedure :: insertAtNodeValue
    end type avl

contains

    !Subrutinas del tipo avl
    ! subroutine insert(self, val)
    !     class(avl), intent(inout) :: self
    !     integer, intent(in) :: val

    !     call insertRec(self%root, val)
    ! end subroutine insert

    subroutine insert(self, val, capas)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%root, val, capas)
    end subroutine insert

    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%root => deleteRec(self%root, val)
    end subroutine

    subroutine preorder(self)
        class(avl), intent(in) :: self

        call preorderRec(self%root)
    end subroutine preorder

    subroutine graph(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        command = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"


        write(io, *) "digraph G {"
        if(associated(self%root)) then
            call printRec(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(command, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if

    end subroutine graph
```

En el arbol avl se realizan diferentes operaciones para queal insertar un valor nuevo al arbol se verifique primero si el arbol esta balanceado o necesita rotar antes de insertar el nuevo nodo.

## matriz.f90

Este archivo es el que contiene la estructura de la matriz dispersa, donde lo más extenso es la funciónd e graficar. Dicha función se trato de optimizar lo mayor posible ya que el tiempo de convertir el archivo .dot a png era un poco alto.

```fortran
module matrix_m
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        character(len=:), allocatable :: value
    end type node_val

    type :: node
        private
        integer :: i, j
        character(len=:), allocatable :: value
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        procedure :: graficar
    end type

contains

    subroutine graficar(self,filename)
        class(matrix), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: fila,f
        integer :: colum,c
        type(node), pointer :: aux
        type(node_val) :: val
        integer :: unit


        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Matriz {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos

        aux => self%root%down

        ! Todo se grafica fila por fila -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

        ! Se imprimen los encabezados de las columnas
        do fila=-1, self%width
            if (fila == -1) then
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="root"]'

                ! Union con el primer nodo de la fila
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node_fila_-1_0','"[dir=back]'

            else
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila-1, '_', 0,'" -> "Node_encabezado_', fila, '_', 0, '"[dir=back]'

                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0, '" [label="',fila,'"]'

                ! Union de encabezados con los nodos de la fila
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"'
                write(unit, '(*(g0))') '"Node_encabezado_', fila, '_', 0,'" -> "Node', 0,'_',fila,'"[dir=back]'



            end if
        end do

        ! Se establecen que nodos conforman una fila
        write(unit, '(*(g0))') ' '
        write(unit, '(*(g0))', advance="no") '{rank=same;'
        do fila=-1, self%width
            write(unit, '(*(g0))',advance="no") '"Node_encabezado_', fila, '_', 0, '";'
        end do
        write(unit, '(*(g0))',advance="yes") '};'

        ! salto de linea para que este tan pegado
        write(unit, '(*(g0))') ' '


        ! Se inicializa j
        fila=0
        f=0

        do fila = 0, self%height
            f=fila
            colum = 0
            ! write(*, fmt='(I3)', advance='no') i

            ! Inicio de la fila
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" [label="',fila,'"]'

            !Union de las filas
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"'
            write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node', fila,'_',colum,'"[dir=back]'

            !Union con flechas a la derecha
            if(.not. fila== self%height) then
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"'
                write(unit, '(*(g0))') '"Node_fila_', fila-1,"_",colum,'" -> "Node_fila_', fila,'_',colum,'"[dir=back]'
            end if


            do colum = 0, self%width
                val = self%getValue(fila,colum)

                if(.not. val%exists) then
                    ! write(*, fmt='(I3)', advance='no') 0
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="',colum,'", group=', fila,']'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label="", group=', fila,']'

                else
                    ! write(*, fmt='(L3)', advance='no') val%value

                    write(unit, '(*(g0))') '"Node', fila,"_",colum,'" [label= " ", fillcolor="',val%value,'", group=', fila,']'

                end if

                if (.not. colum==0) then
                    !Union de las filas izquierda y derecha
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"'
                    write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila,'_',colum,'"[dir=back]'
                end if

                ! Union arriba y abajo
                if (.not. colum==0 ) then

                    if (.not. fila==self%height) then
                    ! Union arriba y abajo
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum-1,'" -> "Node', fila+1,'_',colum-1,'"[dir=back]'
                    end if
                end if

                ! Union arriba y abajo del ultimo nodo
                if (colum==self%width) then
                    if (.not. fila==self%height) then
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"'
                        write(unit, '(*(g0))') '"Node', fila,"_",colum,'" -> "Node', fila+1,'_',colum,'"[dir=back]'
                    end if
                end if

            end do

            ! === Se establecen que nodos conforman una fila ===

            ! salto de linea para que este tan pegado
            write(unit, '(*(g0))') ' '
            write(unit, '(*(g0))', advance="no") '{rank=same;'
            c=0
            do c=-1, self%width
                if (c==-1)then
                    write(unit, '(*(g0))',advance="no") '"Node_fila_', f-1, '_', c+1, '";'
                else
                    write(unit, '(*(g0))',advance="no") '"Node', f,"_",c,'";'
                end if
            end do
            write(unit, '(*(g0))',advance="yes") '};'

            write(unit, '(*(g0))') ' '

            ! {rank=same;"Node_fila_-1_0";"Node0_0","Node0_1","Node0_2","Node0_3","Node0_4","Node0_5","Node0_6","Node0_7","Node0_8","Node0_9","Node0_10";};

        end do


        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)

        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'


    end subroutine graficar
```
