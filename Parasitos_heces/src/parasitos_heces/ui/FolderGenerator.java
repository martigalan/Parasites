package parasitos_heces.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

public class FolderGenerator {
	
	private static String archivoDiccionario = "correspondencia.txt";

	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		System.out.println("Dame el nombre de la carpeta. Tiene formato {nombre}_{fecha}:");
		String theoricFolderName = scanner.nextLine();
		File currentWorkingDirectory = new File("./");
		File[] foldersResultByName = currentWorkingDirectory.listFiles((dir, name) -> name.toLowerCase().contains(theoricFolderName)); //guardo fotos de mi carpeta
		
		if (foldersResultByName.length == 0) {
		    System.out.println("No se han encontrado elementos bajo el nombre " + theoricFolderName); 
		    return;
		} 
		for (File file : foldersResultByName) {
			
			if (file.isDirectory()) {
				HashMap<String,String> diccionario = leerDiccionarioCSV(archivoDiccionario,file); //diccionario con nombres archivos anteriores vs originales
				obtenerRutasDiccionario(diccionario, file);
			}
			
		}
	}
	
	private static HashMap<String, String> leerDiccionarioCSV(String nombreArchivo, File ruta) {
        HashMap<String, String> diccionario = new HashMap<>();
        File rutaCorrespondencia = new File(ruta,nombreArchivo); //va al workspace a por mi carpeta
        try (BufferedReader reader = new BufferedReader(new FileReader(rutaCorrespondencia))) {
            String linea;
            while ((linea = reader.readLine()) != null) {
                String[] partes = linea.split(";");
                diccionario.put(partes[1], partes[0]); //guarda como clave: realId..., valor: nombre original
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return diccionario;
    }
	
	private static void obtenerRutasDiccionario(HashMap<String,String> diccionario, File ruta) {
		renombrarArchivos(diccionario, ruta);
	}
	
	//recorrer los files, buscar en el diccionario el nuevo nombre y cambiarlo por el que ya tienen en nueva carpeta 
	private static void renombrarArchivos(HashMap<String,String> diccionario, File ruta) {
        String nombreOriginal = "generado";
        File nombreOrig = new File(ruta.getParent(),ruta.getName()+nombreOriginal);
         
        if (!nombreOrig.exists()) nombreOrig.mkdir();
	    // Crear la carpeta donde se guardarán los archivos con sus nombres originales
	    for (File archivo : ruta.listFiles((dir, name) -> !(name.contains(".txt")) || dir.isDirectory())) {
	    	try{
	    	var kkk = (archivo.getName().split(Constantes.REAL_ID)[1].split(("\\.")));
	    	var nombreBueno = Constantes.REAL_ID+(kkk[0]);
	    	var nombreArchivoOriginal = diccionario.get(nombreBueno)+"."+kkk[1];

	        if (!nombreOriginal.isEmpty()) {
	        	Path rutaOriginal = archivo.toPath();
	        	File rutaNueva = new File(nombreOrig, nombreArchivoOriginal);
                try {
                    Files.copy(rutaOriginal, rutaNueva.toPath(), StandardCopyOption.REPLACE_EXISTING);
                } catch (IOException e) {
                    e.printStackTrace();
                }
	        }}catch (Exception e) {
				// TODO: handle exception
			}
	    }
	}
}
	
