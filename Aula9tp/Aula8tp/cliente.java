import java.io.*;
import java.net.*;

public class cliente {
    public static void main(String[] args) {
        if (args.length < 3) {
            System.err.println("Uso: java cliente <ip_servidor> <porta> <nick>");
            System.exit(1);
        }

        String serverAddress = args[0];
        int port = Integer.parseInt(args[1]);
        String nick = args[2];

        try {
            Socket socket = new Socket(serverAddress, port);
            Thread senderThread = new Thread(new Sender(socket, nick));
            senderThread.start();
        } catch (UnknownHostException e) {
            System.err.println("Endereço do servidor desconhecido: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Erro de I/O ao conectar-se ao servidor: " + e.getMessage());
        }
    }
}

class Sender implements Runnable {
    private Socket socket;
    private String nick;

    public Sender(Socket socket, String nick) {
        this.socket = socket;
        this.nick = nick;
    }

    @Override
    public void run() {
        try {
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            BufferedReader consoleReader = new BufferedReader(new InputStreamReader(System.in));

            String message;
            while ((message = consoleReader.readLine()) != null) {
                out.println(nick + ":" + message);
            }

            consoleReader.close();
            out.close();
            socket.close();
        } catch (IOException e) {
            System.err.println("Erro de I/O no cliente: " + e.getMessage());
        }
    }
}
