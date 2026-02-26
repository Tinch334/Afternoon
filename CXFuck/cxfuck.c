#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


bool validChar(char c) {
    return (c == '<' || c == '>' || c == '+' || c == '-' || c == '.' || c == ',' ||  c == '[' || c == ']');
}

// Get only relevant file contents.
char *getFileContents(FILE *fptr) {
    int c;
    size_t size = 100;
    size_t ptr = 0;
    char *res = malloc(size * sizeof(char));

    while ((c = fgetc(fptr)) != EOF) {
        if (validChar (c)) {
            //Check if buffer is full.
            if (ptr >= size - 1) {
                size *= 2;
                res = realloc(res, size);
            }

            res[ptr++] = (char)c;
        }
    }

    res[ptr] = '\0';
    return res;
}

void convert(char *program, FILE *fptr) {
    size_t loopStack[strlen(program)];
    size_t loopStackPtr = 0;

    size_t loopCounter = 0;
    size_t ptr = 0;

    while (program[ptr] != '\0') {
        switch(program[ptr]) {
        case '>':
            fputs("incq %rax\n", fptr);
            break;

        case '<':
            fputs("decq %rax\n", fptr);
            break;

        case '+':
            fputs("incb (%rax)\n", fptr);
            break;

        case '-':
            fputs("decb (%rax)\n", fptr);
            break;

        case '.':
            fputs("movq %rax, %rbx\n", fptr);

            fputs("movzbl (%rax), %edi\n", fptr); // Move byte to argument register, extending with zero.
            fputs("call putchar\n", fptr);

            fputs("movq %rbx, %rax\n", fptr);
            break;

        case ',':
            fputs("movq %rax, %rbx\n", fptr);

            fputs("call getchar\n", fptr);
            fputs("movb %al, (%rbx)\n", fptr);

            fputs("movq %rbx, %rax\n", fptr);
            break;

        case '[':
            // Check for jump to loop end.
            fputs("cmpb $0, (%rax)\n", fptr);
            fprintf(fptr, "jz LC%ld\n", loopCounter);
            // Adds a label on the opening bracket of a loop.
            fprintf(fptr, "LO%ld:\n", loopCounter);

            loopStack[loopStackPtr++] = loopCounter++;;

            break;
        case ']':
            {
                // Get matching closing brace.
                size_t loopNumber = loopStack[--loopStackPtr];

                // Check for unmatched braces.
                if (loopStackPtr < 0)
                    exit(-1);

                // Check for jump to loop end.
                fputs("cmpb $0, (%rax)\n", fptr);
                fprintf(fptr, "jnz LO%ld\n", loopNumber);
                fprintf(fptr, "LC%ld:\n", loopNumber);

                break;
            }
        }

        ptr++;
    }
}


int main(int argc, char const *argv[])
{
    FILE *fptr;

    fptr = fopen(argv[1], "r");
    // Attempt to open file and read significant contents.
    if (fptr == NULL)
        return -1;

    char *contents = getFileContents(fptr);

    for (char *c = contents; *c != '\0'; c++)
        printf("%c", *c);
    printf("\n");

    fclose(fptr);

    fptr = fopen("runner.s", "w");
    if (fptr == NULL)
        return -1;

    // Write program start to file.
    fputs(".data\n", fptr);
    fputs("printStr: .asciz \"%c\"\n", fptr);
    fputs("mem: .zero 100000\n", fptr);
    fputs(".text\n", fptr);
    fputs(".global main\n", fptr);
    fputs("main:\n", fptr);

    fputs("pushq %rbp\n", fptr);
    fputs("movq %rsp, %rbp\n", fptr);
    // We use %rbp and it's callee saved.
    fputs("pushq %rbp\n", fptr);

    fputs("leaq mem(%rip), %rax\n", fptr);

    convert(contents, fptr);

    fputs("xorl %eax, %eax\n", fptr);
    fputs("popq %rbx\n", fptr); // Restore rbx.
    fputs("popq %rbp\n", fptr);
    fputs("ret\n", fptr);
    fputs(".section .note.GNU-stack,\"\",@progbits\n", fptr); // Safety.

    fclose(fptr);
    free(contents);

    return 0;
}