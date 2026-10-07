using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Windows.Forms;

internal static class LinkEDALauncher
{
    private const string MutexName = "Local\\LinkEDA.StandaloneLauncher";

    [STAThread]
    private static int Main()
    {
        string root = AppDomain.CurrentDomain.BaseDirectory;
        try
        {
            Dictionary<string, string> settings = ReadSettings(
                Path.Combine(root, "LinkEDA-runtime.conf"));
            string rHome = Required(settings, "R_HOME");
            string rscript = Required(settings, "RSCRIPT");
            string library = Required(settings, "R_LIBS_USER");
            string version = Required(settings, "LINKEDA_VERSION");
            string script = Path.Combine(root, "LinkEDA-Launcher.R");

            if (!File.Exists(rscript))
                throw new FileNotFoundException(
                    "No se encuentra el R configurado para LinkEDA. Vuelva a ejecutar el instalador.",
                    rscript);
            if (!File.Exists(script))
                throw new FileNotFoundException("Falta el lanzador de LinkEDA.", script);

            bool ownsMutex;
            using (Mutex mutex = new Mutex(true, MutexName, out ownsMutex))
            {
                if (!ownsMutex)
                {
                    if (!ShowExistingWindow(version))
                        MessageBox.Show(
                            "LinkEDA ya se está ejecutando, pero no ha respondido todavía.",
                            "LinkEDA", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return 0;
                }

                Directory.CreateDirectory(library);
                ProcessStartInfo start = new ProcessStartInfo();
                start.FileName = rscript;
                start.Arguments = "--vanilla --no-save --no-restore " + Quote(script);
                start.WorkingDirectory = Environment.GetFolderPath(
                    Environment.SpecialFolder.MyDocuments);
                start.UseShellExecute = false;
                start.CreateNoWindow = true;
                start.EnvironmentVariables["R_HOME"] = rHome;
                start.EnvironmentVariables["R_LIBS_USER"] = library;
                start.EnvironmentVariables["LINKEDA_STANDALONE"] = "1";
                start.EnvironmentVariables["LINKEDA_VERSION"] = version;

                using (Process process = Process.Start(start))
                {
                    if (process == null)
                        throw new InvalidOperationException("No se pudo iniciar R para LinkEDA.");
                    process.WaitForExit();
                    if (process.ExitCode != 0)
                        throw new InvalidOperationException(
                            "R no pudo iniciar LinkEDA. Vuelva a ejecutar el instalador para reparar la instalación.");
                }
            }
            return 0;
        }
        catch (Exception error)
        {
            MessageBox.Show(error.Message, "LinkEDA", MessageBoxButtons.OK,
                MessageBoxIcon.Error);
            return 1;
        }
    }

    private static Dictionary<string, string> ReadSettings(string path)
    {
        if (!File.Exists(path))
            throw new FileNotFoundException(
                "Falta la configuración de LinkEDA. Vuelva a ejecutar el instalador.", path);
        Dictionary<string, string> settings =
            new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (string raw in File.ReadAllLines(path, Encoding.UTF8))
        {
            string line = raw.Trim();
            if (line.Length == 0 || line.StartsWith("#", StringComparison.Ordinal))
                continue;
            int separator = line.IndexOf('=');
            if (separator <= 0) continue;
            settings[line.Substring(0, separator).Trim()] =
                line.Substring(separator + 1).Trim();
        }
        return settings;
    }

    private static string Required(Dictionary<string, string> settings, string name)
    {
        string value;
        if (!settings.TryGetValue(name, out value) || String.IsNullOrWhiteSpace(value))
            throw new InvalidDataException("Falta " + name + " en la configuración de LinkEDA.");
        return value;
    }

    private static string Quote(string value)
    {
        return "\"" + value.Replace("\"", "\\\"") + "\"";
    }

    private static bool ShowExistingWindow(string version)
    {
        try
        {
            using (TcpClient client = new TcpClient())
            {
                IAsyncResult pending = client.BeginConnect("127.0.0.1", 39072, null, null);
                if (!pending.AsyncWaitHandle.WaitOne(TimeSpan.FromSeconds(2))) return false;
                client.EndConnect(pending);
                using (NetworkStream stream = client.GetStream())
                using (StreamWriter writer = new StreamWriter(stream, new UTF8Encoding(false)))
                using (StreamReader reader = new StreamReader(stream, Encoding.UTF8))
                {
                    writer.NewLine = "\n";
                    writer.WriteLine("WELCOME_LAUNCH");
                    writer.WriteLine("standalone");
                    writer.WriteLine(version);
                    writer.WriteLine("none");
                    writer.WriteLine("__LINKEDA_TCP_MESSAGE_END__");
                    writer.Flush();
                    string reply = reader.ReadLine();
                    return reply != null && !reply.StartsWith("ERR ", StringComparison.Ordinal);
                }
            }
        }
        catch
        {
            return false;
        }
    }
}
