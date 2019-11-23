using System;


using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Security;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualBasic;


using System.Windows.Forms;

namespace CAOS30
{
    

    public class Corazon
    {
        public string message = "";
        private OpenFileDialog ofd = new OpenFileDialog();
        private SaveFileDialog sfd = new SaveFileDialog();
        private string fileName, AppName, Secction, privado, publico, rootFile;
        private long numbersLine;
        private string[] valor = new string[3];
        
        public ProgressBar obj1 = new ProgressBar();


       

        private long[] toDec(string a)
        {
            string[] array;
            long[] r = new long[4];
            int i;


            array = a.Split('-');
            if (array.Length == 3)
            {
                try
                {
                    for (i = 0; i <= array.Length - 1; i++)
                        r[i] = Convert.ToInt32(array[i], 16);
                }
                catch (Exception ex)
                {
                    r[0] = 0;
                    r[1] = 0;
                    r[2] = 0;
                }
            }
            else
            {
                r[0] = 0;
                r[1] = 0;
                r[2] = 0;
            }
            return r;
        }
        private bool validar_encriptado(long[] p)
        {
            
            if (p[0] == 0 & p[1] == 0)
            {
                //Interaction.MsgBox("Incorrect Password please rewrite it", MsgBoxStyle.Critical, "Error");
                message= "Incorrect Password please rewrite it";
                return false;
            }
            else
                return true;
        }

        private bool validar_desencriptado(long[] e, long[] d)
        {
            

            if ((e[0] == 0 & e[1] == 0) | (d[0] == 0 & d[1] == 0))
            {
                //Interaction.MsgBox("Incorrect Passwords please rewrite these", MsgBoxStyle.Critical, "Error");

                message = "Incorrect Passwords please rewrite these Error";
                return false;
            }
            else if (e[1] != d[1])
            {
                //Interaction.MsgBox("Incorrect Passwords please rewrite these", MsgBoxStyle.Critical, "Error");
                message= "Incorrect Passwords please rewrite these Error";
                return false;
            }
            else if (((e[0] * d[0]) % e[2]) == 1)
                return true;
            else
            {
                //Interaction.MsgBox("Incorrect Passwords please rewrite these", MsgBoxStyle.Critical, "Error");
                message= "Incorrect Passwords please rewrite these Error";
                return false;
            }
        }



        private long ExpModular(long a, long n, long m)
        {
            long exp=0, x=0;
            double aux=0;

            exp = 1;
            x = a % m;

            while (n > 0)
            {
                if ((n % 2) != 0)
                    exp = (exp * x) % m;
                aux = Math.Pow(x, 2);
                x = (Convert.ToInt64(aux) % m); //antes x
                
                n = (n / 2);
            }
            return exp;
        }

        private string leer_archivo()
        {
            string cad;
            int n, i;
            ofd.Filter = "txt|*.txt|ini|*.ini|log|*.log|inf|*.inf|doc|*.doc";


            //ofd.ShowDialog();

            if (ofd.ShowDialog() == DialogResult.OK)
            {
                rootFile = ofd.FileName;
            }



            //FileOpen(1, ofd.FileName, OpenMode.Input);



            n = 0;

            cad = ofd.FileName;
            // cad = "D:\primavera\odio el silencio.txt"
            i = cad.Length - 1;

            while (n == 0)
            {
                if (cad[i].ToString() == @"\")
                    n = i;
                i = i - 1;
            }

            cad = cad.Substring( n + 2, cad.Length - n);
            return cad;
        }

        private string Cerrar_Archivo(string texto)
        {
            string cad;
            int n, i;

            //FileSystem.FileClose(1);
            sfd.Filter = "Archivos de texto txt|*.txt|doc|*.doc";
            sfd.ShowDialog();
            n = 0;

            if (sfd.FileName != null)
            {
                FileOpen(2, sfd.FileName, OpenMode.Output);
                FileSystem.PrintLine(2, texto);
                FileSystem.FileClose(2);
                // setear_claves(sfd.FileName)
                Interaction.MsgBox("Archivo almacenado");
            }

            cad = sfd.FileName;
            // cad = "D:\primavera\odio el silencio.txt"
            i = cad.Length - 1;

            while (n == 0)
            {
                if (cad[i].ToString() == @"\")
                    n = i;
                i = i - 1;
            }

            cad = cad.Substring( n + 2, cad.Length - n);
            return cad;
        }



        private string[] Get_words(string linea, char separador)
        {
            string[] palabras;
            int i;

            string tex;

            palabras = linea.Split(separador);


            for (i = 0; i <= palabras.Length - 1; i++)
            {
                if (palabras[i] == "")
                    palabras[i] = " ";
            }

            return palabras;
        }

        private string[] Translate2Number(string[] arrayWords)
        {
            string[] numeros;
            string cadena;
            char[] caracteres_palabra;
            int n;

            numeros = new string[arrayWords.Length - 1 + 1];
            cadena = "";

            int i, j;

            for (i = 0; i <= arrayWords.Length - 1; i++)
            {
                caracteres_palabra = arrayWords[i].ToCharArray();
                for (j = 0; j <= caracteres_palabra.Length - 1; j++)
                {

                    //Convert.ToInt32(Char)
                    //n = Strings.Asc(caracteres_palabra[j]);

                    n = Convert.ToInt32(caracteres_palabra[j]);

                    cadena += n + " ";
                }
                //cadena = Strings.RTrim(cadena);
                cadena = cadena.TrimEnd();


                numeros[i] = cadena;
                cadena = "";
                caracteres_palabra = null;
            }

            return numeros;
        }
        private string[] Number2Translate(string[] arrayNumeros)
        {
            string[] palabras;
            string cadena;
            string[] cod_num;
            int n;
            char letra;

            palabras = new string[arrayNumeros.Length - 1 + 1];
            cadena = "";

            int i, j;

            for (i = 0; i <= arrayNumeros.Length - 1; i++)
            {
                cod_num = arrayNumeros[i].Split(' ');
                for (j = 0; j <= cod_num.Length - 1; j++)
                {
                    n = Convert.ToInt32(cod_num[j]);

                    //letra = Strings.Chr(n);
                    letra = Convert.ToChar(n);

                    cadena += letra;
                }

                palabras[i] = cadena;
                cadena = "";
                cod_num = null;
            }

            return palabras;
        }

        private string[] Number2Code(long n, long m, string[] arrayNumbers)
        {
            int i, j, valor;
            long codigo;
            string[] codificados, valores;
            string cadena;

            codificados = new string[arrayNumbers.Length - 1 + 1];
            cadena = "";

            for (i = 0; i <= arrayNumbers.Length - 1; i++)
            {
                valores = arrayNumbers[i].Split(' ');
                for (j = 0; j <= valores.Length - 1; j++)
                {
                    valor = Convert.ToInt32(valores[j]);
                    codigo = ExpModular(valor, n, m);
                    cadena += codigo + " ";
                }
                cadena = cadena.TrimEnd();
                codificados[i] = cadena;
                cadena = "";
            }
            return codificados;
        }
        private string[] Code2Number(long n, long m, string[] arrayCifrado)
        {
            int i, j, valor;
            long codigo;
            string[] decodificados, valores;
            string cadena;

            decodificados = new string[arrayCifrado.Length - 1 + 1];
            cadena = "";

            for (i = 0; i <= arrayCifrado.Length - 1; i++)
            {
                valores = arrayCifrado[i].Split(' ');
                for (j = 0; j <= valores.Length - 1; j++)
                {
                    codigo = Convert.ToInt64(valores[j]);
                    valor = Convert.ToInt32(ExpModular(codigo, n, m));
                    cadena += valor + " ";
                }
                cadena = cadena.TrimEnd();
                decodificados[i] = cadena;
                cadena = "";
            }
            return decodificados;
        }
        private string NewLine(string[] arrayCodigo, char separador)
        {
            int i;
            string cadena;
            cadena = "";

            for (i = 0; i <= arrayCodigo.Length - 1; i++)
            {
                if (i == arrayCodigo.Length - 1)
                    cadena += arrayCodigo[i];
                else
                    cadena += arrayCodigo[i] + separador;
            }


            return cadena;
        }

        public void Encriptacion(string llavePublica, ref ProgressControl obj2)
        {
            long[] p;
            string texto, linea;
            string[] arrayPalabras, arrayNumeros, arrayCodigo;
            int n;




            texto = "";
            linea = "";
            p = toDec(llavePublica);

            if (validar_encriptado(p) == true)
            {
                fileName = leer_archivo();

                contarLineas(rootFile);

                obj2.Show();
                obj1.GroupBox1.Visible = true;


                n = 0;
                while (!FileSystem.EOF(1))
                {
                    Progreso(obj2, "Encriptando Archivo", n, numbersLine);
                    arrayPalabras = Get_words(FileSystem.LineInput(1), " ");
                    arrayNumeros = Translate2Number(arrayPalabras);
                    arrayPalabras = null;
                    arrayCodigo = Number2Code(p[0], p[1], arrayNumeros);
                    arrayNumeros = null;
                    linea = NewLine(arrayCodigo, "@");


                    arrayCodigo = null;
                    texto += linea + Constants.vbNewLine;
                    n = n + 1;
                }

                Cerrar_Archivo(texto);
            }
        }

        public void Desencriptador(string llavePublica, string llavePrivada, ref ProgressControl obj2)
        {
            long n, m;
            string nombreArchivo;
            long nLine;
            nLine = 0;

            long[] e, d;
            string texto, linea, scrip;
            string[] arrayPalabras, arrayNumeros, arrayCifrado;
            texto = "";
            linea = "";
            e = toDec(llavePublica);
            d = toDec(llavePrivada);
            n = d[0];
            m = d[1];
            if (validar_desencriptado(e, d) == true)
            {
                fileName = leer_archivo();
                contarLineas(rootFile);

                obj2.Show();
                obj1.GroupBox1.Visible = true;


                while (!FileSystem.EOF(1))
                {
                    Progreso(obj2, "DESencriptando Archivo", nLine, numbersLine);

                    scrip = Strings.Trim(FileSystem.LineInput(1));
                    if (scrip != "")
                    {
                        arrayCifrado = Get_words(scrip, "@");
                        arrayNumeros = Code2Number(n, m, arrayCifrado);
                        arrayCifrado = null;
                        arrayPalabras = Number2Translate(arrayNumeros);
                        arrayNumeros = null;
                        linea = NewLine(arrayPalabras, " ");
                        arrayPalabras = null;
                        texto += linea + Constants.vbNewLine;
                    }
                    nLine = nLine + 1;
                }
                nombreArchivo = Cerrar_Archivo(texto);
            }
        }



        public void setear_claves(string key1, string key2)
        {
            AppName = "Aplicacion de Prueba";
            Secction = "Llaves del Sistema";
            Interaction.SaveSetting(AppName, Secction, "key1", key1);
            Interaction.SaveSetting(AppName, Secction, "key2", key2);
        }
        public void reset_claves()
        {
            PublicFuncions obj = new PublicFuncions();
            string[] claves = new string[2];
            claves = obj.generar_claves_usuario;
            setear_claves(claves[0], claves[1]);
        }
        public string[] obtener_claves()
        {
            string prueba;
            string[] a = new string[2];
            AppName = "Aplicacion de Prueba";
            Secction = "Llaves del Sistema";
            prueba = Interaction.GetSetting(AppName, Secction, "key1");
            if (prueba == "")
            {
                a[0] = "nada";
                a[1] = "nada";
            }
            else
            {
                a[0] = prueba;
                a[1] = Interaction.GetSetting(AppName, Secction, "key2");
            }
            return a;
        }
        private void contarLineas(string root)
        {
            long n;
            n = 0;

            while (!FileSystem.EOF(1))
            {
                n = n + 1;
                FileSystem.LineInput(1);
            }

            FileSystem.FileClose(1);
            FileSystem.FileOpen(1, root, OpenMode.Input);
            numbersLine = n;
        }

        private void Progreso(ProgressControl obj, string title, long n, long totalLines)
        {
            long porcentaje;

            obj.Label1.Text = title;

            if (n == 0)
                obj.Label2.Text = "0%";
            else
            {
                porcentaje = (n * 100) / (double)totalLines;
                obj.Label2.Text = Convert.ToString(porcentaje) + "%";
                obj.ProgressBar1.Value = Convert.ToString(porcentaje);
            }
        }

        public Corazon()
        {
        }
    }

}
