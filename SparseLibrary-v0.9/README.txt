:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
             - Instrucciones de uso - Sparse Library - v0.9 -    
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

	Para utilizar la biblioteca sparse de fortran, se debe compilar 
junto con el/los programa/s propio/s siguiendo la secuencia:

[compilador] [programas propios.f90] [path a sparseLib.a] -I[path 
a la carpeta Objects de la biblioteca] -o [nombre del ejecutable a crear]

Ejemplo:
	Utilizando gfortran, colocando la carpeta extraida del archivo zip
en la misma carpeta del programa y llamando 'main' al ejecutable, la linea
de comando a ejecutar en la terminal seria:

gfortran main.f90 SparseLibrary-v0.9/sparseLib.a -ISparseLibrary-v0.9
/Objects -o main

	 Para opciones de compilacion avanzadas visitar: 
https://gcc.gnu.org/wiki/GFortranGettingStarted, y
https://gcc.gnu.org/wiki/GFortranUsage

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
				 08/2019			
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
