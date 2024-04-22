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

    character(:), allocatable  :: color, album_js
    character(len=1000) :: filename
    character(len=1000) :: filename_capas
    character(:), allocatable :: dpi,nombre_cliente, password_cliente

    integer :: i, p, size, pixels, num,id_json  
    logical :: found
    integer, allocatable :: capas_aux(:)

    ! Matriz dispersa para las capas
    type(matrix) :: matriz_c

    ! Arbol BB
    type(abb) :: tree_abb

    ! Lista de albumes
    type(lista_circular) :: lista_albumes

    ! Lista de clientes
    type(cola) :: cola_clientes

    !Arbol AVL
    type(avl) :: tree_avl

    integer :: opcion

    ! Asignar valores a las variables admin y password
    admin = "admin"
    password = "EDD2024"
    
    ! Ciclo para el menú
    do
        call mostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case(1)
                ! El usuario ingresa su nombre de usuario y contraseña
                print *, "Ingrese su nombre de usuario:"
                read(*, *) usuario
                print *, "Ingrese su contraseña:"
                read(*, *) clave
                
                ! Verificar si el nombre de usuario y la contraseña son del administrador
                if (usuario == admin .and. clave == password) then
                    print *, "*** Bienvenido administrador ***"
                    call mostrarMenuAdmin()
                else
                    ! print *, "Nombre de usuario o contraseña incorrectos"
                    print *, "*** Bienvenido cliente ***"
                    call mostrarMenuCliente()
                end if

            case(2)
                print *, "Registro de usuarios"

                call registrar_cliente()

            case(3)
                print *, "Saliendo..."
                exit

            case default
                print *, "Opcion invalida. Por favor, seleccione una opcion valida."

        end select
    end do

contains

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

    subroutine mostrarMenuAdmin()
        implicit none
        integer :: opcionAdmin
        do
            print *, " "
            print *, "============ Menu Administrador ============"
            print *, "Seleccione una opcion:"
            print *, "1. Arbol B de usuarios (Grafico)"
            print *, "2. Crear nuevo usuario"
            print *, "3. Modificar usuario"
            print *, "4. Eliminar usuario"
            print *, "5. Carga masiva de usuarios"
            print *, "6. Salir"
            print *, "==========================================="
            read(*, *) opcionAdmin
            select case(opcionAdmin)
                case(1)
                    print *, "Mostrando Arbol B de usuarios..."

                    call cola_clientes%graficar('grafica_clientes.dot')

                    call abrir_grafica_usuarios()
                case(2)
                    print *, "=== Crear nuevo usuario ==="

                    call registrar_cliente()

                case(3)
                    print *, "=== Modificar usuario ==="

                    call modificar_usuario()
                case(4)

                    print *, "=== Eliminar usuario ==="

                    call eliminar_usuario()
                    
                case(5)
                    print *, "=== Carga masiva de usuarios ==="
                    
                    ! El usuario ingresa la direccion del JSON
                    print *, "=== Ingrese la direccion del archivo JSON de clientes: ==="
                    read(*, '(A1000)') filename !Ingresar direccion de archivo JSON
                    ! filename= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
                    ! EDD_PROYECTO_202044192\Fase2\clientes.json"

                    call json%initialize()    ! Se inicializa el módulo JSON
                    call json%load(filename=trim(filename))  !se carga el archivo de entrada Json
                    ! call json%print()         ! Imprimir
                    print *, "Archivo cargado exitosamente."
                    
                    call json%info('',n_children=size)

                    call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                    call json%get('', listPointer, found)

                    do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                        call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
                        call jsonc%get_child(id_capa_pointer, 'dpi', attributePointer, found = found) 
                        call jsonc%get_child(id_capa_pointer, 'nombre_cliente', nombre_c_pointer, found = found)
                        call jsonc%get_child(id_capa_pointer, 'password', password_c_pointer, found = found)

                        if (found) then
                                                
                            call jsonc%get(attributePointer, dpi)          
                            call jsonc%get(nombre_c_pointer, nombre_cliente)                                                       
                            call jsonc%get(password_c_pointer, password_cliente)

                            call cola_clientes%append(nombre_cliente, dpi, password_cliente)

                        end if
                    end do

                    call json%destroy() ! Se finaliza el módulo JSON

                    ! call cola_clientes%graficar('cola_clientes.dot')
                case(6)
                    exit
                case default
                    print *, "Opción inválida, por favor seleccione nuevamente."
            end select
        end do
    end subroutine mostrarMenuAdmin

    subroutine cargarCapas()
        implicit none

        ! El usuario ingresa la direccion del JSON
        print *, "=== Ingrese la direccion del archivo JSON de capas: ==="
        read(*, '(A1000)') filename_capas !Ingresar direccion de archivo JSON
        ! filename_capas= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
        ! EDD_PROYECTO_202044192\Fase2\capas.json"

        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename=trim(filename_capas))  !se carga el archivo de entrada Json
        ! call json%print()         ! Imprimir
        print *, "Archivo cargado exitosamente."
                    
        call json%info('',n_children=size)

        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)

        ! print *, "Nombres detectados:"
        do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(id_capa_pointer, 'id_capa', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombrdel hijo actual
            
            if (found) then 
                call jsonc%get(attributePointer,id_capa)
                ! Insertar capas en el arbol b
                call tree_abb%insert(id_capa)
            end if

            call jsonc%get_child(id_capa_pointer, 'pixeles', attributePointer, found = found)
            call jsonc%info(attributePointer, n_children=pixels)

            do p = 1, pixels
                call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)
                if (found) then

                    call jsonc%get(pixelsPointer, "color", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, color)
                    end if
                    
                    call jsonc%get(pixelsPointer, "columna", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, colum)
                    end if

                    call jsonc%get(pixelsPointer, "fila", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, row)
                    end if
                end if

                call matriz_c%insert(colum, row, color,id_capa)
                ! print *, "Capa: ", id_capa, "Color: ", color, "Columna: ", colum, "Fila: ", row

            end do
        end do

        !graficar el arbol b de capas
        call tree_abb%graph("arbol_b_capas")

        call json%destroy() ! Se finaliza el módulo JSON

    end subroutine cargarCapas


    subroutine cargar_albumes()
        implicit none

        ! El usuario ingresa la direccion del JSON
        print *, "=== Ingrese la direccion del archivo JSON de albumes: ==="
        read(*, '(A1000)') filename_capas !Ingresar direccion de archivo JSON
        ! filename_capas= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
        ! EDD_PROYECTO_202044192\Fase2\albumes.json"

        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename=trim(filename_capas))  !se carga el archivo de entrada Json
        ! call json%print()         ! Imprimir
        print *, "Archivo de albumes cargado exitosamente."
                    
        call json%info('',n_children=size)

        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)

        ! print *, "Nombres detectados:"
        do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(id_capa_pointer, 'nombre_album', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombrdel hijo actual
            
            if (found) then 
                call jsonc%get(attributePointer,album_js)
                ! Insertar capas en el arbol b
                ! print *, "Album: ", album_js

                call lista_albumes%append(i,album_js)
            end if

            call jsonc%get_child(id_capa_pointer, 'imgs', attributePointer, found = found)       
            

                if (found) then
                    call jsonc%info(attributePointer, n_children=pixels)  ! Obtiene el número de elementos en el array 'imgs'

                    do p = 1, pixels
                        call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)  ! Obtiene el p-ésimo elemento del array 'imgs'

                        if (found) then
                            call jsonc%get(pixelsPointer, num)  ! Lee el número
                            ! print *, "Número: ", num
                            call lista_albumes%append_img(album_js,num)
                        end if
                    end do
                end if
        end do
        !     call jsonc%info(attributePointer, n_children=pixels)

        !     do p = 1, pixels
        !         call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)
                
        !         if (found) then

        !             call jsonc%get(pixelsPointer, "color", pixelAttribute, found= found)
        !             if (found)then 
        !                 call jsonc%get(pixelAttribute, color)
        !             end if
                    
        !         end if

        !     end do
        ! end do

        call json%destroy() ! Se finaliza el módulo JSON

        call lista_albumes%graficar('lista_album.dot')

    end subroutine cargar_albumes

    subroutine cargar_imgs()
        implicit none

        ! El usuario ingresa la direccion del JSON
        print *, "=== Ingrese la direccion del archivo JSON de imagenes: ==="
        read(*, '(A1000)') filename_capas !Ingresar direccion de archivo JSON
        ! filename_capas= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
        ! EDD_PROYECTO_202044192\Fase2\img.json"

        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename=trim(filename_capas))  !se carga el archivo de entrada Json
        ! call json%print()         ! Imprimir
        print *, "Archivo de imgs cargado exitosamente."
                    
        call json%info('',n_children=size)

        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)

        ! print *, "Nombres detectados:"
        do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(id_capa_pointer, 'id', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombrdel hijo actual
            
            if (found) then 
                call jsonc%get(attributePointer,id_json)
                ! print *, "ID: ", id_json
            end if

            call jsonc%get_child(id_capa_pointer, 'capas', attributePointer, found = found)       
            

                if (found) then
                    call jsonc%info(attributePointer, n_children=pixels)  ! Obtiene el número de elementos en el array 'capas'

                    ! call tree_avl%insert(id_json, pixels)

                    allocate(capas_aux(pixels))

                    do p = 1, pixels
                        call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)  ! Obtiene el p-ésimo elemento del array 'imgs'

                        if (found) then
                            call jsonc%get(pixelsPointer, num)  ! Lee el número
                            capas_aux(p) = num
                        end if
                    end do

                    ! print *, "Capas: ", capas_aux
                    call tree_avl%insert(id_json,capas_aux)
                    deallocate(capas_aux)
                end if
        end do

        call json%destroy() ! Se finaliza el módulo JSON

        call tree_avl%graph()
        ! call tree_avl%preorder()

    end subroutine cargar_imgs

    subroutine registrar_cliente()
        character(len=100) :: nombre_cliente
        character(len=100) :: password_cliente
        character(len=100) :: dpi

        print *, "Ingrese el nombre del usuario:"
        read(*, *) nombre_cliente

        print *, "Ingrese el DPI del usuario:"
        read(*, *) dpi

        print *, "Ingrese la contraseña del usuario:"
        read(*, *) password_cliente

        call cola_clientes%append(nombre_cliente, dpi, password_cliente)

    end subroutine registrar_cliente

    subroutine modificar_usuario()
        character(len=100) :: nombre_cliente
        character(len=100) :: password_cliente
        character(len=100) :: dpi

        print *, "Ingrese el DPI del usuario a modificar:"
        read(*, *) dpi

        print *, "Ingrese el nuevo nombre del usuario:"
        read(*, *) nombre_cliente

        print *, "Ingrese la nueva contraseña del usuario:"
        read(*, *) password_cliente

        call cola_clientes%modificar_dpi(dpi,nombre_cliente,password_cliente)

    end subroutine modificar_usuario

    subroutine eliminar_usuario()
        character(len=100) :: nombre_u

        print *, "Ingrese el nombre del usuario a eliminar:"
        read(*, '(A1000)') nombre_u

        call cola_clientes%delete(nombre_u)

    end subroutine eliminar_usuario

    subroutine abrir_grafica_usuarios()
        character(len=100) :: command

        ! Reemplaza 'nombre_archivo.pdf' con la ruta de tu archivo PDF
        command = "grafica_clientes.dot.png"
        call system(command)

    end subroutine abrir_grafica_usuarios

    subroutine abrir_grafica(graf)
        character(len=100) :: command
        character(len=*), intent(in) :: graf

        ! Reemplaza 'nombre_archivo.pdf' con la ruta de tu archivo PDF
        command = graf
        call system(command)

    end subroutine abrir_grafica

    subroutine img_por_capa()
        implicit none

        integer, dimension(:), allocatable :: capas_input
        integer :: num_capas, i
        character(len=100) :: input_line

        print *, "Ingrese el numero de capas (total de capas a graficar):"
        read(*, *) num_capas
        allocate(capas_input(num_capas))

        print *, "Ingrese los IDs de las capas separados por espacios:"
        read(*, '(A)') input_line

        ! Parsear la línea de entrada para obtener los IDs
        read(input_line, *) capas_input

        call matriz_c%graficarCapas("capas_prueba.dot", capas_input, " ",1)

        deallocate(capas_input)
    end subroutine img_por_capa

    subroutine img_preorden()
        implicit none
        integer, dimension(:), allocatable :: preorder_values

        preorder_values = tree_abb%preorder_get()

        print *, "Valores en preorden: ", preorder_values
        call matriz_c%graficarCapas("preorden.dot", preorder_values, "Preorden",400)

        deallocate(preorder_values)

    end subroutine img_preorden

    subroutine img_inorden()
        implicit none
        integer, dimension(:), allocatable :: inorden_values

        inorden_values = tree_abb%inorder_get()

        print *, "Valores en inorden: ", inorden_values
        call matriz_c%graficarCapas("inorden.dot", inorden_values, "Inorden",400)

        deallocate(inorden_values)

    end subroutine img_inorden

    subroutine img_postorden()
        implicit none
        integer, dimension(:), allocatable :: postorden_values

        postorden_values = tree_abb%postorder_get()

        print *, "Valores en postorden: ", postorden_values
        call matriz_c%graficarCapas("postorden.dot", postorden_values, "Postorden",400)

        deallocate(postorden_values)

    end subroutine img_postorden

    subroutine reporte()
        implicit none
        print *, "=== Preorden: ==="
        call tree_abb%preorder()
        print *, "=== Inorden: ==="
        call tree_abb%inorder()
        print *, "=== Postorden: ==="
        call tree_abb%posorder()

    end subroutine reporte

end program main
