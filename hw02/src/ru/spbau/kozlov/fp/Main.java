package ru.spbau.kozlov.fp;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

public class Main {

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader(args[0]));
             PrintWriter writer = new PrintWriter(args[1])) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.replace("=", ".").toLowerCase();

                writer.printf("F = Y (\\ %s)\n", line);
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
