using System;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Reflection;

internal static class InstallerBootstrap
{
    private const string PayloadName = "LinkEDA.Payload";

    private static int Main()
    {
        string extracted = Path.Combine(Path.GetTempPath(),
            "LinkEDA-install-" + Guid.NewGuid().ToString("N"));
        try
        {
            Console.WriteLine("Preparando la instalacion de LinkEDA...");
            Directory.CreateDirectory(extracted);
            ExtractPayload(extracted);

            string installer = Path.Combine(extracted, "Instalar-LinkEDA-Windows.ps1");
            if (!File.Exists(installer))
                throw new InvalidDataException("Falta el instalador de LinkEDA en el paquete.");

            ProcessStartInfo start = new ProcessStartInfo();
            start.FileName = "powershell.exe";
            start.Arguments = "-NoProfile -ExecutionPolicy Bypass -File \"" + installer + "\"";
            start.UseShellExecute = false;
            using (Process process = Process.Start(start))
            {
                if (process == null)
                    throw new InvalidOperationException("No se pudo iniciar el instalador.");
                process.WaitForExit();
                if (process.ExitCode != 0)
                    throw new InvalidOperationException(
                        "La instalacion no ha terminado. Revise el mensaje anterior.");
            }
            Console.WriteLine("LinkEDA esta listo. Puede abrirlo desde el escritorio.");
            return 0;
        }
        catch (Exception error)
        {
            Console.Error.WriteLine("Error: " + error.Message);
            if (!Console.IsInputRedirected)
            {
                Console.WriteLine("Pulse una tecla para cerrar esta ventana.");
                Console.ReadKey(true);
            }
            return 1;
        }
        finally
        {
            try
            {
                if (Directory.Exists(extracted)) Directory.Delete(extracted, true);
            }
            catch (IOException) { }
            catch (UnauthorizedAccessException) { }
        }
    }

    private static void ExtractPayload(string extracted)
    {
        Stream payload = Assembly.GetExecutingAssembly()
            .GetManifestResourceStream(PayloadName);
        if (payload == null)
            throw new InvalidDataException("El instalador no contiene el paquete de LinkEDA.");
        using (payload)
        using (ZipArchive archive = new ZipArchive(payload, ZipArchiveMode.Read))
        {
            string root = Path.GetFullPath(extracted)
                .TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar;
            foreach (ZipArchiveEntry entry in archive.Entries)
            {
                string destination = Path.GetFullPath(Path.Combine(
                    extracted, entry.FullName.Replace('/', Path.DirectorySeparatorChar)));
                if (!destination.StartsWith(root, StringComparison.OrdinalIgnoreCase))
                    throw new InvalidDataException("El paquete contiene una ruta no valida.");
                if (entry.FullName.EndsWith("/", StringComparison.Ordinal))
                {
                    Directory.CreateDirectory(destination);
                    continue;
                }
                Directory.CreateDirectory(Path.GetDirectoryName(destination));
                using (Stream source = entry.Open())
                using (FileStream target = File.Create(destination))
                    source.CopyTo(target);
            }
        }
    }
}
