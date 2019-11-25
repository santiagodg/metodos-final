clear

///////////////////////////////////////////////////////////////////////////
////////////////////////////    Montante   ////////////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = Montante(dMatrix)
    iRows = size(dMatrix)(1);
    iCols = size(dMatrix)(2);
    for i = 1 : iRows
        if i == 1 then
            iPivoteAnterior = 1;
        else
            iPivoteAnterior = dMatrix(i - 1, i - 1);
        end
        for k = 1 : iRows
            if k <> i then
                if k < i then
                    dMatrix(k, k) = dMatrix(i, i);
                end
                for j = i + 1 : iCols
                    dMatrix(k, j) = (dMatrix(i, i) * dMatrix(k, j) - dMatrix(i, j) * dMatrix(k, i)) / iPivoteAnterior;
                end
                dMatrix(k, i) = 0;
            end
        end
    end
    for i = 1 : iRows
        dMatrix(i, iCols) = dMatrix(i, iCols) / dMatrix(i, i);
        dMatrix(i, i) = dMatrix(i, i) / dMatrix(i, i);
    end
    dX = dMatrix(:, iCols)';
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////   Obtener Datos    /////////////////////////////
///////////////////////////////////////////////////////////////////////////

function matriz = ObtenerDatos(sArchivo)
    hojasExcel = readxls(sArchivo + ".xls");
    hojaDatos = hojasExcel(1);
    matriz = hojaDatos(:, :);
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////   Regresión Lineal    ////////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = RegresionLineal(dMatrizDatos)
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
    dX = Montante(matrizEcuaciones);
endfunction

///////////////////////////////////////////////////////////////////////////
//////////////////////////  Regresión Cuadrática  /////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = RegresionCuadratica(dMatrizDatos)
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
    dX = Montante(matrizEcuaciones);
endfunction


///////////////////////////////////////////////////////////////////////////
//////////////////////////  Regresión Exponencial  ////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = RegresionExponencial(dMatrizDatos)
    // Calcular solucion
    iN = size(dMatrizDatos)(1);
    dSumaX = 0;
    dSumaX2 = 0;
    dSumaLnY = 0;
    dSumaXLnY = 0;
    for iI = 1 : iN
        dSumaX = dSumaX + dMatrizDatos(iI, 1);
        dSumaX2 = dSumaX2 + dMatrizDatos(iI, 1)^2;
        dSumaLnY = dSumaLnY + log(dMatrizDatos(iI, 2));
        dSumaXLnY = dSumaXLnY + dMatrizDatos(iI, 1) * log(dMatrizDatos(iI, 2));
    end
    matrizEcuaciones = [  iN   , dSumaX , dSumaLnY;
                        dSumaX , dSumaX2, dSumaXLnY];
    dX = Montante(matrizEcuaciones);
    dX(1) = %e ^ dX(1);
endfunction

///////////////////////////////////////////////////////////////////////////
//////////////////////////  Regresión Potencia  ///////////////////////////
///////////////////////////////////////////////////////////////////////////

function dX = RegresionPotencial(dMatrizDatos)
    // Calcular solucion
    iN = size(dMatrizDatos)(1);
    dSumaLnX = 0;
    dSumaLnX2 = 0;
    dSumaLnY = 0;
    dSumaLnXLnY = 0;
    for iI = 1 : iN
        dSumaLnX = dSumaLnX + log(dMatrizDatos(iI, 1));
        dSumaLnX2 = dSumaLnX2 + log(dMatrizDatos(iI, 1))^2;
        dSumaLnY = dSumaLnY + log(dMatrizDatos(iI, 2));
        dSumaLnXLnY = dSumaLnXLnY + log(dMatrizDatos(iI, 1)) * log(dMatrizDatos(iI, 2));
    end
    matrizEcuaciones = [  iN     , dSumaLnX , dSumaLnY   ;
                        dSumaLnX , dSumaLnX2, dSumaLnXLnY];
    dX = Montante(matrizEcuaciones);
    dX(1) = %e ^ dX(1);
endfunction

///////////////////////////////////////////////////////////////////////////
//////////////////////////////   R Cuadrada   /////////////////////////////
///////////////////////////////////////////////////////////////////////////

function dR2 = R2(dMatrizDatos, sRegresion, dX)
    iN = size(dMatrizDatos)(1);
    dYProm = mean(dMatrizDatos(:, 2));
    dSST = 0;
    for iI = 1 : iN
        dSST = dSST + (dMatrizDatos(iI, 2) - dYProm) ^ 2;
    end
    
    if sRegresion == "lineal" | sRegresion == "l" then
        for iI = 1 : iN
            dValoresEsperados(iI) = dX(1) + dX(2) * dMatrizDatos(iI, 1);
        end
    elseif sRegresion == "cuadratica" | sRegresion == "c" then
        for iI = 1 : iN
            dValoresEsperados(iI) = dX(1) + dX(2) * dMatrizDatos(iI, 1) + dX(3) * dMatrizDatos(iI, 1) ^ 2;
        end
    elseif sRegresion == "exponencial" | sRegresion == "e" then
        for iI = 1 : iN
            dValoresEsperados(iI) = dX(1) * %e ^ (dX(2) * dMatrizDatos(iI, 1));
        end
    elseif sRegresion == "potencial" | sRegresion == "p" then
        for iI = 1 : iN
            dValoresEsperados(iI) = dX(1) * dMatrizDatos(iI, 1) ^ dX(2);
        end
    end
    
    dSSE = 0;
    for iI = 1 : iN
        dSSE = dSSE + (dMatrizDatos(iI, 2) - dValoresEsperados(iI)) ^ 2;
    end
    
    dR2 = (dSST - dSSE) / dSST;
endfunction

///////////////////////////////////////////////////////////////////////////
/////////////////////////   Valores Atípicos   ////////////////////////////
///////////////////////////////////////////////////////////////////////////

function dValoresAtipicos = Atipicos(dMatrizDatos, sRegresion, dX)
    iN = size(dMatrizDatos)(1);
    if sRegresion == "lineal" | sRegresion == "l" then
        for iI = 1 : iN
            dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dX(1) + dX(2) * dMatrizDatos(iI, 1))) ^ 2;
        end
    elseif sRegresion == "cuadrática" | sRegresion == "c" then
        for iI = 1 : iN
            dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dX(1) + dX(2) * dMatrizDatos(iI, 1) + dX(3) * dMatrizDatos(iI, 1) ^ 2)) ^ 2;
        end
    elseif sRegresion == "exponencial" | sRegresion == "e" then
        for iI = 1 : iN
            dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dX(1) * %e ^ (dX(2) * dMatrizDatos(iI, 1)))) ^ 2;
        end
    elseif sRegresion == "potencial" | sRegresion == "p" then
        for iI = 1 : iN
            dErroresCuadrados(iI) = (dMatrizDatos(iI, 2) - (dX(1) * dMatrizDatos(iI, 1) ^ dX(2))) ^ 2;
        end
    end
    
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
    dValoresAtipicos = list();
    if iContadorAtipicos <> 1 then
        for iI = 1 : iContadorAtipicos - 1
            dValoresAtipicos(iI) = dMatrizDatos(dIndicesAtipicos(iI));
        end
    end
end

///////////////////////////////////////////////////////////////////////////
////////////////////////////   Vector A String   //////////////////////////
///////////////////////////////////////////////////////////////////////////

function sValores = VectorAString(dListaDatos, sDelimitador)
    if sDelimitador == "p" | sDelimitador == "parentesis" then
        sValores = "(";
    elseif sDelimitador == "c" | sDelimitador == "corchetes" then
        sValores = "[";
    elseif sDelimitador == "l" | sDelimitador == "llaves" then
        sValores = "{";
    end
    for iI = 1 : length(dListaDatos)
        if iI == length(dListaDatos) then
            if sDelimitador == "p" | sDelimitador == "parentesis" then
                sValores = sValores + string(dListaDatos(length(dListaDatos))) + ")";
            elseif sDelimitador == "c" | sDelimitador == "corchetes" then
                sValores = sValores + string(dListaDatos(length(dListaDatos))) + "]";
            elseif sDelimitador == "l" | sDelimitador == "llaves" then
                sValores = sValores + string(dListaDatos(length(dListaDatos))) + "}";
            end
        else
            sValores = sValores + string(dListaDatos(iI)) + ", ";
        end
    end
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////////   Mejor Modelo   /////////////////////////////
///////////////////////////////////////////////////////////////////////////

function sMejorModelo = MejorModelo(dR2)
    sRegresiones = ["lineal", "cuadrática", "exponencial", "potencial"];
    [m, k] = max(dR2);
    sMejorModelo = sRegresiones(k);
endfunction

///////////////////////////////////////////////////////////////////////////
///////////////////////////   EvaluarRegresion   //////////////////////////
///////////////////////////////////////////////////////////////////////////

function dY = EvaluarRegresion(sRegresion, dA, dX)
    dY = [];
    if sRegresion == "lineal" | sRegresion == "l" then
        for iI = 1 : length(dX)
            dY(iI) = dA(1) + dA(2) * dX(iI);
        end
    elseif sRegresion == "cuadrática" | sRegresion == "c" then
        for iI = 1 : length(dX)
            dY(iI) = dA(1) + dA(2) * dX(iI) + dA(3) * dX(iI) ^ 2;
        end
    elseif sRegresion == "exponencial" | sRegresion == "e" then
        for iI = 1 : length(dX)
            dY(iI) = dA(1) * %e ^ (dA(2) * dX(iI));
        end
    elseif sRegresion == "potencial" | sRegresion == "p" then
        for iI = 1 : length(dX)
            dY(iI) = dA(1) * dX(iI) ^ dA(2);
        end
    end
endfunction

///////////////////////////////////////////////////////////////////////////
////////////////////////////   Plotea Todo   //////////////////////////////
///////////////////////////////////////////////////////////////////////////

function PloteaTodo(dMatrizDatos, dXs)
    clf();
    plot2d(dMatrizDatos(:, 1)', dMatrizDatos(:, 2)', style=-2);
    
    dOrdenadosX = gsort(dMatrizDatos(:, 1)', "g", "i");
    dOrdenadosY = gsort(dMatrizDatos(:, 2)', "g", "i");
    
    dSepPromX = 0;
    dSepPromY = 0;
    for iI = 1 : length(dOrdenadosX) - 1
        dSepPromX = dSepPromX + abs(dOrdenadosX(iI) - dOrdenadosX(iI + 1));
        dSepPromY = dSepPromY + abs(dOrdenadosY(iI) - dOrdenadosY(iI + 1));
    end
    dSepPromX = dSepPromX / (length(dOrdenadosX) - 1);
    dSepPromY = dSepPromY / (length(dOrdenadosY) - 1);
    xmin = dOrdenadosX(1) - dSepPromX;
    ymin = dOrdenadosY(1) - dSepPromY;
    xmax = dOrdenadosX(length(dOrdenadosX)) + dSepPromX;
    ymax = dOrdenadosY(length(dOrdenadosY)) + dSepPromY;
    
    dValoresX = xmin : dSepPromX / 10 : xmax;
    
    plot2d(dValoresX, [EvaluarRegresion("l", dXs(1), dValoresX), EvaluarRegresion("c", dXs(2), dValoresX), EvaluarRegresion("e", dXs(3), dValoresX), EvaluarRegresion("p", dXs(4), dValoresX)], style=[5, 3, 2, 6]);
    legend(["Datos", "Lineal", "Cuadrática", "Exponencial", "Potencial"]);
    zoom_rect([xmin, ymin, xmax, ymax]);
    xgrid(1, 1, 7);
endfunction

///////////////////////////////////////////////////////////////////////////
/////////////////////////   Programa Principal   //////////////////////////
///////////////////////////////////////////////////////////////////////////

disp("");
sArchivoExcel = input("Ingrese el nombre del archivo .xls que desea analizar: ", "s");
dMatrizDatos = ObtenerDatos(sArchivoExcel);

mprintf("Conjunto de valores x: %s\n", VectorAString(dMatrizDatos(:, 1)', "c"));
mprintf("Conjunto de valores y: %s\n", VectorAString(dMatrizDatos(:, 2)', "c"));
dValorAEstimar = input("¿Para qué valor desea estimar? ");

dXL = RegresionLineal(dMatrizDatos);
dXC = RegresionCuadratica(dMatrizDatos);
dXE = RegresionExponencial(dMatrizDatos);
dXP = RegresionPotencial(dMatrizDatos);

dR2L = R2(dMatrizDatos, "l", dXL);
dR2C = R2(dMatrizDatos, "c", dXC);
dR2E = R2(dMatrizDatos, "e", dXE);
dR2P = R2(dMatrizDatos, "p", dXP);

mprintf("I) Modelos:\n\n");
mprintf("\t- Lineal      :   y = (%f) + (%f) * x\n", dXL(1), dXL(2));
mprintf("\t                  r^2 = %f\n\n", dR2L);
mprintf("\t- Cuadrática  :   y = (%f) + (%f) * x + (%f) * x^2\n", dXC(1), dXC(2), dXC(3));
mprintf("\t                  r^2 = %f\n\n", dR2C);
mprintf("\t- Exponencial :   y = (%f) * e ^ ((%f) * x)\n", dXE(1), dXE(2));
mprintf("\t                  r^2 = %f\n\n", dR2E);
mprintf("\t- Potencial   :   y = (%f) * x ^ (%f)\n", dXP(1), dXP(2));
mprintf("\t                  r^2 = %f\n\n", dR2P);

mprintf("II) Conclusiones:\n\n");
sMejorModelo = MejorModelo([dR2L, dR2C, dR2E, dR2P]);
mprintf("\t- El mejor modelo será la %s, con una r^2 de %f\n\n", sMejorModelo, max([dR2L, dR2C, dR2E, dR2P]));
mprintf("\t- Usando cada modelo, los valores estimados para x = %f serán:\n\n", dValorAEstimar);
mprintf("\t\t- Lineal      :   %f\n\n", EvaluarRegresion("l", dXL, dValorAEstimar));
mprintf("\t\t- Cuadrática  :   %f\n\n", EvaluarRegresion("c", dXC, dValorAEstimar));
mprintf("\t\t- Exponencial :   %f\n\n", EvaluarRegresion("e", dXE, dValorAEstimar));
mprintf("\t\t- Potencial   :   %f\n\n", EvaluarRegresion("p", dXP, dValorAEstimar));

PloteaTodo(dMatrizDatos, list(dXL, dXC, dXE, dXP));

if sMejorModelo == "lineal" then
    dValoresAtipicos = Atipicos(dMatrizDatos, "l", dXL);
elseif sMejorModelo == "cuadrática" then
    dValoresAtipicos = Atipicos(dMatrizDatos, "c", dXC);
elseif sMejorModelo == "exponencial" then
    dValoresAtipicos = Atipicos(dMatrizDatos, "e", dXE);
elseif sMejorModelo == "potencial" then
    dValoresAtipicos = Atipicos(dMatrizDatos, "p", dXP);
end

if dValoresAtipicos == list() then
    mprintf("\t- De acuerdo con los cuadrados de las distancias entre cada\n");
    mprintf("\t  punto y el modelo %s, no existen valores atípicos.\n\n", sMejorModelo);
else
    mprintf("\t- De acuerdo con los cuadrados de las distancias entre cada\n");
    mprintf("\t  punto y el modelo %s, existen valores anormales:\n\n", sMejorModelo);
    mprintf("\t\t%s\n\n", VectorAString(dValoresAtipicos, "p"));
end


///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
