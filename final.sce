clear

///////////////////////////////////////////////////////////////////////////
////////////////////////////    Montante   ////////////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = Montante(dMatrix)
    // Determina tamaño de la matriz
    iRows = size(dMatrix)(1);
    iCols = size(dMatrix)(2);

    // Itera sobre cada fila
    for i = 1 : iRows

        // Determina el pivote anterior
        if i == 1 then
            iPivoteAnterior = 1;
        else
            iPivoteAnterior = dMatrix(i - 1, i - 1);
        end

        // Itera sobre cada fila
        for k = 1 : iRows

            // Para toda fila diferente a la de nuestro pivote
            if k <> i then

                // Si la fila es menor a la del pivote
                if k < i then

                    // Asigna a la diagonal el valor del pivote
                    dMatrix(k, k) = dMatrix(i, i);
                end

                // Itera sobre todas las columnas adelante del pivote
                for j = i + 1 : iCols

                    // Determinante dividido entre el pivote
                    dMatrix(k, j) = (dMatrix(i, i) * dMatrix(k, j) - dMatrix(i, j) * dMatrix(k, i)) / iPivoteAnterior;
                end

                // Asigna cero a toda la columna del pivote menos al pivote
                dMatrix(k, i) = 0;
            end
        end
    end

    // Matriz identidad
    for i = 1 : iRows
        dMatrix(i, iCols) = dMatrix(i, iCols) / dMatrix(i, i);
        dMatrix(i, i) = dMatrix(i, i) / dMatrix(i, i);
    end

    // Devolver solución
    dX = dMatrix(:, iCols)';
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////   Obtener Valores    /////////////////////////////
///////////////////////////////////////////////////////////////////////////

function matriz = ObtenerValores(sArchivo)
    hojasExcel = readxls(sArchivo);
    hojaDatos = hojasExcel(1);
    matriz = hojaDatos(:, :);
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////   Regresión Lineal    ////////////////////////////
///////////////////////////////////////////////////////////////////////////

function [dCoefs, dR2, dValoresAtipicos] = RegresionLineal(dMatrizDatos)
    // Calcular solucion
    iN = size(dMatrizDatos)(1);
    dSumaX = 0;
    dSumaY = 0;
    dSumaX2 = 0;
    dSumaXY = 0;
    for iI = 1 : iN
        dSumaX = dSumaX + dMatrizDatos(iI, 1);
        dSumaY = dSumaY + dMatrizDatos(iI, 2);
        dSumaX2 = dSumaX2 + dMatrizDatos(iI, 1)^2;
        dSumaXY = dSumaXY + dMatrizDatos(iI, 1) * dMatrizDatos(iI, 2);
    end
    matrizEcuaciones = [  iN  , dSumaX , dSumaY ;
                        dSumaX, dSumaX2, dSumaXY];
    dCoefs = Montante(matrizEcuaciones);

    // Calcular r cuadrada
    dYProm = mean(dMatrizDatos(2));
    dSST = 0;
    for iI = 1 : iN
        dSST = dSST + (dMatrizDatos(iI, 2) - dYProm)^2;
    end

    dSSE = 0;
    for iI = 1 : iN
        dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dCoefs(1) + dCoefs(2) * dMatrizDatos(iI, 1)))^2;
        dSSE = dSSE + dErroresCuadrados(iI);
    end
    dR2 = (dSST - dSSE) / dSST;

    // Buscar valores atípicos
    iContadorAtipicos = 1;
    dCuartil1 = quart(dErroresCuadrados)(1);
    dCuartil3 = quart(dErroresCuadrados)(3);
    dIQR = dCuartil3 - dCuartil1;
    for iI = 1 : iN
        if dErroresCuadrados(iI) < dCuartil1 - 1.5 * dIQR || dCuartil3 + 1.5 * dIQR < dErroresCuadrados(iI) then
            dIndicesAtipicos(iContadorAtipicos) = iI;
            iContadorAtipicos = iContadorAtipicos + 1;
        end
    end
    dValoresAtipicos = 12345678;
    if iContadorAtipicos <> 1 then
        for iI = 1 : iContadorAtipicos - 1
            dValoresAtipicos(iI) = dMatrizDatos(dIndicesAtipicos(iI));
        end
    end
endfunction

///////////////////////////////////////////////////////////////////////////
//////////////////////////  Regresión Cuadrática  /////////////////////////
///////////////////////////////////////////////////////////////////////////

function [dCoefs, dR2, dValoresAtipicos] = RegresionCuadratica(dMatrizDatos)
    // Calcular solucion
    iN = size(dMatrizDatos)(1);
    dSumaX = 0;
    dSumaX2 = 0;
    dSumaX3 = 0;
    dSumaX4 = 0;
    dSumaY = 0;
    dSumaXY = 0;
    dSumaX2Y = 0;
    for iI = 1 : iN
        dSumaX = dSumaX + dMatrizDatos(iI, 1);
        dSumaX2 = dSumaX2 + dMatrizDatos(iI, 1)^2;
        dSumaX3 = dSumaX3 + dMatrizDatos(iI, 1)^3;
        dSumaX4 = dSumaX4 + dMatrizDatos(iI, 1)^4;
        dSumaY = dSumaY + dMatrizDatos(iI, 2);
        dSumaXY = dSumaXY + dMatrizDatos(iI, 1) * dMatrizDatos(iI, 2);
        dSumaX2Y = dSumaX2Y + dMatrizDatos(iI, 1)^2 * dMatrizDatos(iI, 2);
    end
    matrizEcuaciones = [  iN   , dSumaX , dSumaX2, dSumaY  ;
                        dSumaX , dSumaX2, dSumaX3, dSumaXY ;
                        dSumaX2, dSumaX3, dSumaX4, dSumaX2Y];
    dCoefs = Montante(matrizEcuaciones);

    // Calcular r cuadrada
    dYProm = mean(dMatrizDatos(2));
    dSST = 0;
    for iI = 1 : iN
        dSST = dSST + (dMatrizDatos(iI, 2) - dYProm)^2;
    end

    dSSE = 0;
    for iI = 1 : iN
        dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dCoefs(1) + dCoefs(2) * dMatrizDatos(iI, 1) + dCoefs(3) * dMatrizDatos(iI, 1)^2))^2;
        dSSE = dSSE + dErroresCuadrados(iI);
    end
    dR2 = (dSST - dSSE) / dSST;

    // Buscar valores atípicos
    iContadorAtipicos = 1;
    dCuartil1 = quart(dErroresCuadrados)(1);
    dCuartil3 = quart(dErroresCuadrados)(3);
    dIQR = dCuartil3 - dCuartil1;
    for iI = 1 : iN
        if dErroresCuadrados(iI) < dCuartil1 - 1.5 * dIQR || dCuartil3 + 1.5 * dIQR < dErroresCuadrados(iI) then
            dIndicesAtipicos(iContadorAtipicos) = iI;
            iContadorAtipicos = iContadorAtipicos + 1;
        end
    end
    dValoresAtipicos = 12345678;
    if iContadorAtipicos <> 1 then
        for iI = 1 : iContadorAtipicos - 1
            dValoresAtipicos(iI) = dMatrizDatos(dIndicesAtipicos(iI));
        end
    end
endfunction

///////////////////////////////////////////////////////////////////////////
/////////////////////////   Programa Principal   //////////////////////////
///////////////////////////////////////////////////////////////////////////

disp("");
sArchivoExcel = input("Ingrese el nombre del archivo que desea analizar: ", "s");
dMatrizDatos = ObtenerValores(sArchivoExcel);
[dCoefsL, dR2L, dAtipicosL] = RegresionLineal(dMatrizDatos);
[dCoefsC, dR2C, dAtipicosC] = RegresionCuadratica(dMatrizDatos);

exponencial = 0;
potencia = 0;

mprintf("   Lineal    :   y = (%f) + (%f) * x\n", dCoefsL(1), dCoefsL(2));
mprintf("                 r^2 = %f\n\n", dR2L);
mprintf(" Cuadratica  :   y = (%f) + (%f) * x + (%f) * x^2\n", dCoefsC(1), dCoefsC(2), dCoefsC(3));
mprintf("                 r^2 = %f\n\n", dR2C);

///////////////////////////////////////////////////////////////////////////
