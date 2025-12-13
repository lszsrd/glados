#include <stdbool.h>
#include <string.h>

bool isOk()
{
    return true;
}

int main(int argc, char **argv)
{
    int i = 1 * 1 + 0;
    float f = i;
    bool *b = NULL;

    f -= 3;
    if (isOk()) {
        i *= 2;
    } else {
        i++;
    }
    if (1 == 2 || 1 == 3) {
        i--;
    }
    for (int j = 0; j < 10; j++) {
        i++;
    }
    return i;
}
