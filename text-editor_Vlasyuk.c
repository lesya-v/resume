#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>

#define ERR_FILE_OPEN 1

#define ERR_ARG_COUNT "-Wrong number of arguments-\n"
#define ERR_OPEN_F "-The program can not open the file-\n"
#define ERR_MEMORY_GAIN "-The program can not gain the memory-\n"

#define PIECE_SIZE 50
#define BUF_SIZE 512
#define PP 256

#define OPEN_ERR 0
#define ERR_COMM "-It's a wrong command. Please, try again-\n"
#define ERR_QUANT_ARG 0

#define BIGBUF_SIZE 100
#define ERR_ADDING_MEMORY 0

enum PRINT_MODE
{
    NORMAL = 0,
    PAGES,
    PAGES2
} print_mode;

enum
{
    KEY_LEFT = 186,
    KEY_RIGHT = 185,
    SPACE = 32, /* Если нажат пробел */
    IN_Q = 113  /* Если нажата Q */
};

typedef struct _list
{
    char *value;
    struct _list *prev; 
    struct _list *next;
} List;

typedef struct _Linf
{
    int size;
    List *head;
    List *tail;
} linf;

linf *lst, *lsth;
char *nmf = NULL;
char *strs = NULL;
char *sw = NULL, *sh = NULL;
int insaf = 0, resub = 0;
int nas = 0;
int az = 0, help = 2, r = 0;
struct winsize w;
struct termios old_attributes,new_attributes;

linf* createlst()
{
    linf *tmp = (linf*)malloc(sizeof(linf));
    if(tmp == NULL) exit(0);
    tmp->size = 0;
    tmp->head = tmp->tail = NULL;
    return tmp;
}

void push(linf *lst, char* str)
{
    int mls = strlen(str) + 1 + PIECE_SIZE;
    List *tmp = (List*)malloc(sizeof(List));
    if(tmp == NULL) exit(0);
    tmp->value = malloc(mls);
    if(tmp->value == NULL) return ;
    tmp->value = strcpy(tmp->value, str);
    tmp->next = NULL;
    tmp->prev = lst->tail;
    if(lst->tail) lst->tail->next = tmp;
    lst->tail = tmp;
    if(lst->head == NULL) lst->head = tmp;
    lst->size++;
}

void pushst(linf *lst, char* str)
{
    int mls = strlen(str) + 1 + PIECE_SIZE;
    List *tmp = (List*)malloc(sizeof(List));
    if(tmp == NULL) exit(0);
    tmp->value = malloc(mls);
    if(tmp->value == NULL) return ;
    tmp->value = strcpy(tmp->value, str);
    tmp->next = lst->head;
    tmp->prev = NULL;
    if(lst->head) lst->head->prev = tmp;
    lst->head = tmp;
    if(lst->tail == NULL) lst->tail = tmp;
    lst->size++;
}

void add_el(linf *lst, FILE* f)
{
    int sz = BUF_SIZE;
    int k = 1;
    char *str;
    char buf[BUF_SIZE];
    str = malloc(BUF_SIZE); 
    if(str == NULL)
    {
        printf(ERR_MEMORY_GAIN);
        exit(0);
    }
    while(1)
    {
        if(fgets(buf, BUF_SIZE - 1, f) == NULL)
        {
            push(lst, str);
            break;
        }
        sz -= strlen(buf);
        while(sz <= 0)
        {
            k++;
            str = realloc(str, k*BUF_SIZE + 2);
            sz += BUF_SIZE;
        }
        str = strcpy(str, buf);
        if(buf[strlen(buf) - 1] != '\n') continue;
        push(lst, str);
        str[0]='\0';
    }
    free(str);
}

List* get_el(linf *lst, int n)
{
    List *el = lst->head;
    int i = 0, cnt = 0;
    if (n < 0) n = 0;
    if (n > lst->size - 1) n = lst->size - 1;
    while ((cnt < lst->size) && i < n)
    {
        el = el->next;
        i++;
        cnt++;
    }
    return el;
}

void* delete_el(linf *lst, int n) 
{
  List *elm = NULL;
  elm = get_el(lst,n);
  if (elm == NULL) exit(0);
  if (elm->prev) elm->prev->next = elm->next;
  if (elm->next) elm->next->prev = elm->prev;
  free(elm->value);
  if (!elm->prev) lst->head = elm->next;
  if (!elm->next) lst->tail = elm->prev;
  
  free(elm);
  lst->size--;
  return elm;
}

void lstdlt(linf *lst)
{
    while(lst->size > 0)
        delete_el(lst, 0);
}

void dltlst(linf **lst)
{
    List *tmp = (*lst)->head;
    List *nxt = NULL;
    int cnt = 0;
    while (cnt < (*lst)->size - 1)
    {
        nxt = tmp->next;
        free(tmp);
        tmp = nxt;
        cnt++;
    }
    free(tmp);
    free(*lst);
    (*lst) = NULL;
}

char* del_str(char* str2, char *str, int n, int m)
{
    int i = 0, j = 0;
    for(i = 0; i < n; i++)
    {
        str2[i] = str[j];
        j++;
    }
    j = m;
    i = n;
    while((i < strlen(str) - (m - n)) && (j < strlen(str)))
    {
        str2[i] = str[j];
        j++;
        i++;
    }
    str2[i] = '\0';
    return str2;
}

int trim(char *str, int ls)
{
    while((str[0] == ' ') || (str[0] == '\t'))
    {
        str = del_str(str, str, 0, 1);
        ls = strlen(str);
    }
    if(ls == 0) return 0;
    while((str[ls - 1] == ' ') || (str[ls - 1] == '\t'))
    {
        str = del_str(str, str, ls - 1, ls);
        ls = strlen(str);
    }
    return ls;
}

void reverse(char *s)
{
    int i = 0, j = 0;
    char c;
    for (i = 0, j = strlen(s) - 1; i < j; i++, j--)
    {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}

char* itoa(int n, char *s)
{
    int i, sign;
    if ((sign = n) < 0)
        n = -n;
    i = 0;
    do
    {
        s[i++] = n % 10 + '0';
    } while ((n /= 10) > 0);
    if(sign < 0)
        s[i++] = '-';
    s[i] = '\0';
    reverse(s);
    return s;
}

int findstr(char *str, char* sw, int ps)
{
    int i = 0, j = 0, ls = strlen(str);
    for(i = ps; i < ls; i++)
    {
        if(str[i] == sw[0])
            for(j = 0; sw[j] != 0; j++)
            {
                if(str[i + j] == sw[j])
                {
                    if(sw[j + 1] == 0)
                        return i;
                }
                else
                    break;
            }
    }
    return -1;
}

int fndsb(char *str, char* sw)
{
    int cntr = 0, ls = strlen(str);
    int i = 0, lsw = strlen(sw);
    while (i < ls)
    {
        if(findstr(str, sw, i) >= 0)
        {
            cntr++;
            i += findstr(str, sw, i) - i + lsw ;
        }
        else i++;
    }
    return cntr;
}

void print_filecontent_lst_option(linf *lsts, int start, int end, int s_start, int s_end, int tab_width)
{
    List *tmp = lsts->head;
    int count = 0, i = 0, len, need = 0, j = 0, tab_count = 0;
    char *str;
    if(start < 0) start = 0;
    if((end > lsts->size - 1) || (end == 0)) end = lsts->size - 1;
    while(tmp)
    {
        str = tmp->value;
        tab_count = 0;
        ++count;
        tab_width = 8;
        i = 0;
        len = 0;
        need = 0;
        j = 0;
        if ((count >= start && count <= end))
        {
            len = strlen((char*)str);
            for(i = 0; str[i] !='\0'; i++)
                if(str[i] == '\t')
                    tab_count++;
            if((s_end <= len) && r != 0)
                len = s_end;
            if(tab_count>0)
            {
                need = len+ tab_count*tab_width;
                if(!s_start)
                    need -= s_end;
                else
                    need -= s_start;
                if(need < 0)
                    need = 0;
            }
            else
                need = 0;
            if(need > 0 && s_start > 0)
            {
                for(i = 0, j = 0; i < len - 1; i++)
                {
                    if(need > 0 && str[j]=='\t')
                    {
                        i += abs(need - tab_width);
                        need-=tab_width;
                    }
                    j++;
                }
                i = j;
            }
            else
            {
                i = s_start;
                j =s_start;
            }
            for( ; i < len - 1; i++)
            {
                if(need > 0 && str[j]=='\t')
                {
                    i += abs(need - tab_width);
                    need-=tab_width;
                }
                printf("%c", str[j]);
                j++;
            }
            if(count != end) printf("\n");
        }
        tmp = tmp->next;
    }
}

int mygetch(void)
{
    struct termios oldt,newt;
    int ch, ch2;
    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    ch2 = getchar();
    if(ch2 == SPACE || ch2 == IN_Q)
    {
        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
        return ch2;
    }
    ch = ch2 + getchar() + getchar();
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    return ch;
}

void print_pages(linf* lsts)
{
    tcgetattr(0,&old_attributes);
    memcpy(&new_attributes,&old_attributes,sizeof(struct termios)); 
    while(1)
    {
        new_attributes.c_lflag &= ~ECHO;
        /*new_attributes.c_lflag &= ~ICANON;
        new_attributes.c_cc[VMIN] = 1;*/
        tcsetattr(0,TCSANOW,&new_attributes);
        ioctl(0, TIOCGWINSZ, &w);
        if(print_mode == PAGES2 || print_mode == PAGES)
        {
            static int size_row = 0, size_row2 = 0,\
            size_col = 0, size_col2 = 0;
            int ch;
            if(print_mode == PAGES2)
            {
                print_mode = PAGES;
                size_row = 0;
                size_row2 = 0;
                size_col = 0;
                size_col2 = 0;
            }
            ch = mygetch();
            size_row = size_row == 0 ? w.ws_row : size_row;
            size_col = size_col == 0 ? w.ws_col : size_col;
            if (ch == '\004')
            {
                printf("\n--EDITOR is closed--\n");
                tcsetattr(0,TCSANOW,&old_attributes);
                free(nmf);
                lstdlt(lst);
                dltlst(&lst);
                if(help == 0)
                {
                    lstdlt(lsth);
                    dltlst(&lsth);
                }
                if(az == 0)
                {
                    free(sw);
                    free(sh);
                }
                free(strs);            
                exit(0);
                break;
            }
            switch (ch)
            {
                case KEY_LEFT:
                {
                    size_col = (size_col - w.ws_col) <= 0 ? w.ws_col : size_col - w.ws_col;
                    size_col2 = (size_col2 - w.ws_col) <= 0 ? 0 : size_col2 - w.ws_col;
                    printf("\n");
                    print_filecontent_lst_option(lsts,  size_row2, size_row, size_col2, size_col, 8);
                    break;
                } 
                case KEY_RIGHT:
                {
                    size_col += w.ws_col;
                    size_col2 += w.ws_col; 
                    printf("\n");
                    print_filecontent_lst_option(lsts,  size_row2, size_row, size_col2, size_col, 8);                
                    break;
                } 
                case IN_Q:
                {
                    print_mode  = NORMAL;
                    printf("\n");
                    tcsetattr(0,TCSANOW,&old_attributes);
                    return;
                    break;
                }
                case SPACE:
                {
                    char fl = 0;
                    size_row += w.ws_row;
                    if(size_row >= lsts->size)
                    {
                        fl = 1;
                        print_mode = NORMAL;
                        size_row = lsts->size -1;
                    }
                    size_row2 += w.ws_row; 
                    printf("\n"); 
                    print_filecontent_lst_option(lsts,  size_row2, size_row, size_col2, size_col, 8);
                    if(fl)
                    {
                        size_row= 0;
                        size_row2 = 0;
                        size_col = 0;
                        size_col2 = 0;
                        printf("\n");
                        if(help != 0) printf("END OF FILE\n");
                        tcsetattr(0,TCSANOW,&old_attributes);
                        return;
                    }
                    break;
                }
            }
        }
    }
    tcsetattr(0,TCSANOW,&old_attributes);
    return;
}

int print_range(linf *lst, char *str)
{
    List* el = NULL;
    int i = 0, fd = 0;
    int n = 0, m = lst->size;
    int ls = strlen(str);
    char *p = NULL;
    ls = trim(str, ls);
    if(ls >= 2)
    {
        for(i = 0; i < ls; i++)
            if(str[i] == '#')
            {
                del_str(str, str, i, ls);
                ls = strlen(str);
                ls = trim(str, ls);
                fd = 1;
                break;
            }
        i = 0;
        if(ls >= 2)
            while(i <= ls - 1)
            {
                if((str[i] == ' ') || (str[i] == '\t'))
                {
                    p = malloc(ls - i + 1);
                    del_str(p, str, i, ls);
                    n = atoi(p);
                    del_str(str, str, 0, i + 1);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    free(p);
                    if((ls < 2)  && (fd == 0)) break;
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    m = atoi(str);
                    break;
                }
                if(str[i] == '\n')
                {
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    n = atoi(str);
                    break;
                }
                i++;
            }
    }
    
    if(lst->size == 0)
    {
        printf("-The empty list-\n");
        return 0;
    }
    if(m <= 0) m = 1;
    if(m > lst->size)
        m=lst->size;
    n--;
    m--;
    if(n < 0) n = 0;
    if(n > lst->size - 1) n = lst->size - 1;
    if (n > m)
    {
        printf(ERR_COMM);
        return 0;
    }
    printf("\n<text>\n\n");
    el = get_el(lst, n);
    if(el == NULL) return 0;
    for(i = n; i <= m; i++)
    {
        if (el == NULL)
        {
            printf(ERR_COMM);
            return 0;
        }
        printf("   %d) %s", i + 1, el->value);
        if (i == m) break;
        else el = el->next;
    }
    printf("\n\n</text>\n");  
    return 0;
}

void insert(linf *lst, int n, char *s)
{
    List *el = NULL;
    List *nel = NULL;
    int mls = strlen(s) + 1 + PIECE_SIZE;
    if(n <= 0)
    {
        pushst(lst, s);
        return ;
    }
    else if(n >= lst->size)
    {
        push(lst, s);
        return ;
    }
    n--;
    el = get_el(lst, n);
    if(el == NULL) return ;
    nel = (List*)malloc(sizeof(List));
    if(nel == NULL) return ;
    nel->value = malloc(mls);
    if (nel->value == NULL) return ;
    nel->value = strcpy(nel->value, s);
    nel->prev = el;
    nel->next = el->next;
    if (el->next) el->next->prev = nel;
    el->next = nel;
    if (!el->prev) lst->head = el;
    if (!el->next) lst->tail = el;
    lst->size++;
}

void insert_after(linf *lst, char *str) 
{
    int i = 0, kk = 0, fn = 0, fk = 0, ffn = 0, ft = 0, fsh = 0;
    int ls = strlen(str), j = 0, lsp = 0, lp = 0;
    char *p = NULL, *spr = NULL;
    trim(str, ls);
    ls = strlen(str);
    if(insaf == 0)
    {
        if (ls <= 2)
        {
            printf(ERR_COMM);
            return ;    
        }
        kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
    
        if(kk < 2)
        {
            printf(ERR_COMM);
            return ;
        }
        kk = 0;
    
        while(1)
        {
            if((str[i] == ' ') || (str[i] == '"'))
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                if(((p[0] > '9') || (p[0] < '0')) && (p[0] != '-'))
                {
                    nas = lst->size;
                    free(p);
                    break;
                }
                nas = atoi(p);
                free(p);
                break;
            }
            i++;
        }
        del_str(str, str, 0, i);
        ls = strlen(str);
        ls = trim(str, ls);
        i = 0;
    }
    else ffn = 1;
    if(strncmp(str, "\"\"\"", strlen("\"\"\"")) == 0) 
    {
        del_str(str, str, 0, strlen("\"\"\""));
        ls = strlen(str);
        ls = trim(str, ls);
        kk = 3;
    }
    
    if((strncmp(str, "\"\"", strlen("\"\"")) == 0) && kk != 3)
    {
        del_str(str, str, 0, strlen("\"\""));
        ls = strlen(str);
        ls = trim(str, ls);
        kk = -1;
    }
    
    if(kk == 0)
    {
        if(insaf == 0)
        {
            if(str[0] == '"')
            {
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
            }
            else
            {
                printf(ERR_COMM);
                return ;
            }
                
            while(1)
            {
                fk = findstr(str, "\"", i);
                if(fk >= 1 && str[fk - 1] == '\\')
                {
                    if((fk >= 0) && (str[fk - 2]) == '\\') break;
                    else i += fk - i + 1;
                }
                else break;
            }
            for(i = fk + 1; i < ls; i++)
                if(str[i] == '#')
                {
                    del_str(str, str, i, ls);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    break;
                }
            i = 0;
            if(str[ls - 1] == '"')
            {
                del_str(str, str, ls - 1, ls);
                ls = strlen(str);
            }
            else
            {
                printf(ERR_COMM);
                return ;
            }
            kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
            if(kk <= 0) kk = 2;
            else
            {
                printf(ERR_COMM);
                return ;
            }
            if(ls < 2)
            {
                insert(lst, nas, "\n\n");
                return ;
            }
        }
        while(i < ls - 1)
        {
            if(str[i] == '\\')
                switch(str[i + 1])
                {
                    case '\\':
                    {
                        fsh = findstr(str, "\\\\", 0);
                        del_str(str, str, fsh, fsh + 1);
                        ls = strlen(str);
                        fsh = 1;
                        continue;
                    }
                    case '"':
                    {
                        if(fsh == 1)
                        {
                            i++;
                            fsh = 0;
                            continue;
                        }
                        fk = findstr(str, "\\\"", 0);
                        del_str(str, str, fk, fk + 1);
                        ls = strlen(str);
                        continue;
                    }
                    case 't':
                    {
                        if(fsh == 1)
                        {
                            i++;
                            fsh = 0;
                            continue;
                        }
                        ft = findstr(str, "\\t", 0);
                        del_str(str, str, ft, ft + 2);
                        ls = strlen(str);
                        for(j = ls + 1; j > ft; j--)
                            str[j] = str[j - 1];
                        str[ft] = '\t';
                        ls = strlen(str);
                        continue;
                    }
                    default:
                    {
                        if((str[i + 1] == 'n') || (str[i + 1] == '0'))
                        {
                            if(fsh == 1)
                            {
                                i++;
                                fsh = 0;
                                continue;
                            }
                            if(str[i + 1] == '\0') fn = findstr(str, "\\0", 0);
                            else
                                fn = findstr(str, "\\n", 0);
                            p = malloc(fn + 3);
                            del_str(p, str, fn, ls);
                            lp = strlen(p);
                            if((ffn == 1) && (nas <= 0))
                                nas = 1;
                            if((nas >= lst->size) && (ffn == 0))
                            {
                                spr = get_el(lst, lst->size - 1)->value;
                                if(spr == NULL) return ;
                                lsp = strlen(spr);
                                if(lsp != 0)
                                {
                                    spr[lsp - 1] = '\n';
                                    spr[lsp] = '\0';
                                }
                                else
                                {
                                    spr[lsp] = '\n';
                                    spr[lsp + 1] = '\0';
                                }
                            }
                            p[lp] = '\n';
                            p[lp + 1] = '\0';
                            insert(lst, nas, p);
                            free(p);
                            del_str(str, str, 0, fn + strlen("\\n"));
                            ls = strlen(str);
                            nas++;
                            i = 0;
                            ffn = 1;
                            continue;
                        }
                        else
                        {
                            del_str(str, str, i, i + 1);
                            ls = strlen(str);
                            continue;
                        }
                    }
                }
            i++;
        }
        
        str[ls] = '\n';
        str[ls + 1] = '\0';
        
        if((nas >= lst->size) && (ffn == 0))
        {
            spr = get_el(lst, lst->size - 1)->value;
            if(spr == NULL) return ;
            lsp = strlen(spr);
            if(lsp != 0)
            {
                spr[lsp - 1] = '\n';
                spr[lsp] = '\0';
            }
            else
            {
                spr[lsp] = '\n';
                spr[lsp + 1] = '\0';
            }
        }
    }
    
    if((insaf == 1) && (kk != 3))
    {
        if(nas <= 0) nas = 0;
        insert(lst, nas, str);
        nas++;
    }
    else if((insaf == 1) && (kk == 3))
    {
        str[ls] = '\n';
        str[ls + 1] = '\0';
        insert(lst, nas, str);
    }
    
    switch(kk)
    {
        case 3:
        {
            if(insaf == 0)
            {
                while(i < ls)
                if(str[i] == '#')
                {
                    del_str(str, str, i, ls);
                    ls = strlen(str);
                    break;
                }
                if(nas >= lst->size)
                {
                    spr = get_el(lst, lst->size - 1)->value;
                    if(spr == NULL) return ;
                    lsp = strlen(spr);
                    if(lsp != 0)
                    {
                        spr[lsp - 1] = '\n';
                        spr[lsp] = '\0';
                    }
                    else
                    {
                        spr[lsp] = '\n';
                        spr[lsp + 1] = '\0';
                    }
                }
            }
            if(insaf == 1) insaf = 0;
            else insaf = 1;
            break;
        }
        case 2:
        {
            insert(lst, nas, str);
            break;
        }
        case -1:
        {
            if(insaf == 0)
            {
                while(i < ls)
                if(str[i] == '#')
                {
                    del_str(str, str, i, ls);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    if(ls < 2) break;
                    else
                    {
                        printf(ERR_COMM);
                        return ;
                    }
                }
                str[ls] = '\n';
                str[ls + 1] = '\0';
                if(nas >= lst->size)
                {
                    spr = get_el(lst, lst->size - 1)->value;
                    if(spr == NULL) return ;
                    lsp = strlen(spr);
                    if(lsp != 0)
                    {
                        spr[lsp - 1] = '\n';
                        spr[lsp] = '\0';
                    }
                    else
                    {
                        spr[lsp] = '\n';
                        spr[lsp + 1] = '\0';
                    }
                }
                insert(lst, nas, str);
                break;
            }
        }
    }
}

int edit_string(linf *lst, char *str)
{
    char *s;
    int ls = strlen(str), i = 0, j = 0;
    int n = 0, m = 0, fl = 0, lss = 0;
    char c = '\0';
    char *p = NULL;
    while((str[0] == ' ') || (str[0] == '\t'))
    {
        str = del_str(str, str, 0, 1);
        ls = strlen(str);
    }
    if(ls < 2)
    {
        printf(ERR_COMM);
        return 0;
    }
    while(i <= ls - 1)
    {
        if(fl == 2)
        {
            if (n < 1) n = 1;
            if (n > lst->size) n = lst->size;
            s = get_el(lst, n - 1)->value;
            if(s == NULL) return 0;
            lss = strlen(s);
            if (m < 1) m = 1;
            if (m > lss) m = lss;
            m--;
            del_str(s, s, m, m + 1);
            lss = strlen(s);
            p = malloc(lss - m + 1);
            p = del_str(p, s, 0, m);
            insert(lst, n, p);
            free(p);
            del_str(s, s, m, lss);
            lss = strlen(s);
            if(lss != 0)
            {
                s[lss - 1] = '\n';
                s[lss] = '\0';
            }
            else
            {
                s[lss] = '\n';
                s[lss + 1] = '\0';
            }
            return 0;
        }
        else if(fl == 3)
        {
            if (n < 1) n = 1;
            if (n > lst->size) n = lst->size;
            s = get_el(lst, n - 1)->value;
            if(s == NULL) return 0;
            lss = strlen(s);
            if (m < 1) m = 1;
            if (m > lss) m = lss;
            m--;
            del_str(s, s, m, lss);
            lss = strlen(s);
            if(lss != 0)
            {
                s[lss - 1] = '\n';
                s[lss] = '\0';
            }
            else
            {
                s[lss] = '\n';
                s[lss + 1] = '\0';
            }
            return 0;
        }
        else if(fl == 4)
        {
            if (n < 1) n = 1;
            if (n > lst->size) n = lst->size;
            s = get_el(lst, n - 1)->value;
            if(s == NULL) return 0;
            lss = strlen(s);
            if (m < 1) m = 1;
            if (m > lss) m = lss;
            m--;
            del_str(s, s, m, m + 1);
            return 0;
        }
        if((str[i] == ' ') || (str[i] == '\t'))
        {
            if(fl == 0)
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                n = atoi(p);
                del_str(str, str, 0, i + 1);
                ls = strlen(str);
                ls = trim(str, ls);
                i = 0;
                fl = 1;
                free(p);
                continue;
            }
            else if(fl == 1)
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                m = atoi(p);
                str = del_str(str, str, 0, i + 1);
                ls = strlen(str);
                ls = trim(str, ls);
                free(p);
                if((strncmp(str, "\\n", 2) == 0) || (strncmp(str, "\'\\n\'", 4) == 0))
                {
                    fl = 2;
                    continue;
                }
                else if((strncmp(str, "\\0", 2) == 0) || (strncmp(str, "\'\\0\'", 4) == 0))
                {
                    fl = 3;
                    continue;
                }
                else if(strncmp(str, "\'\'", 2) == 0)
                {
                    fl = 4;
                    continue;
                }
                else if((strncmp(str, "\\t", 2) == 0) || (strncmp(str, "\'\\t\'", 4) == 0))
                {
                    c = '\t';
                    break;
                }
                else if((strncmp(str, "\\\\", 2) == 0) || (strncmp(str, "\'\\\\\'", 4) == 0))
                {
                    c = '\\';
                    break;
                }
                else if((strncmp(str, "\\\'", 2) == 0) || (strncmp(str, "\'\\\'\'", 4) == 0))
                {
                    c = '\'';
                    break;
                }
                else if(str[0] == '\'') 
                {
                    
                    if((str[2] == '\'') || (ls > 4 && (str[1] == '\\' && str[3] == '\'')))
                    {
                        for(j = findstr(str, "\'", 1); j < ls; j++)
                            if(str[j] == '#')
                            {
                                del_str(str, str, j, ls);
                                ls = strlen(str);
                                ls = trim(str, ls);
                                break;
                            }
                        if(ls > 5)
                        {
                            printf(ERR_COMM);
                            return 0;
                        }
                        if(str[1] == '\\' && str[3] == '\'') c = str[2];
                        else c = str[1];
                        break;
                    }
                    else
                    {
                        printf(ERR_COMM);
                        return 0;
                    }
                }
                else 
                {
                    for(j = 1; j < ls; j++)
                        if(str[j] == '#')
                        {
                            del_str(str, str, j, ls);
                            ls = strlen(str);
                            ls = trim(str, ls);
                            break;
                        }
                    if(ls > 2)
                    {
                        printf(ERR_COMM);
                        return 0;
                    }
                    c = str[0];
                    break;
                }
            }    
        }
        i++;
    }
    
    if (c == '\0')
    {
        printf(ERR_COMM);
        return 0;
    }
    if (n < 1) n = 1;
    if (n > lst->size) n = lst->size;
    s = get_el(lst, n - 1)->value;
    if(s == NULL) return 0;
    ls = strlen(s);
    if (m < 1) m = 1;
    if (m > ls) m = ls - 1;
    s[m - 1] = c;
    return 0;
}

int insert_symbol(linf *lst, char *str)
{
    char *s = NULL;
    int i = 0, ls = strlen(str), j = 0;
    char c = '\0';
    int n = 0, m = 0, fl = 0, lss = 0;
    char *p = NULL;
    while((str[0] == ' ') || (str[0] == '\t'))
    {
        str = del_str(str, str, 0, 1);
        ls = strlen(str);
    }
    if(ls == 0)
    {
        printf(ERR_COMM);
        return 0;
    }
    while(i <= ls - 1)
    {
        if(fl == 2)
        {
            if (n < 1) n = 1;
            if (n > lst->size) n = lst->size;
            s = get_el(lst, n - 1)->value;
            if(s == NULL) return 0;
            lss = strlen(s);
            if (m < 1) m = 1;
            if (m > lss) m = lss;
            m--;
            p = malloc(lss - m + 1);
            p = del_str(p, s, 0, m);
            insert(lst, n, p);
            free(p);
            del_str(s, s, m, lss);
            lss = strlen(s);
            if(lss != 0)
            {
                s[lss - 1] = '\n';
                s[lss] = '\0';
            }
            else
            {
                s[lss] = '\n';
                s[lss + 1] = '\0';
            }
            return 0;
        }
        else if(fl == 3)
        {
            if (n < 1) n = 1;
            if (n > lst->size) n = lst->size;
            s = get_el(lst, n - 1)->value;
            if(s == NULL) return 0;
            lss = strlen(s);
            if (m < 1) m = 1;
            if (m > lss) m = lss;
            m--;
            del_str(s, s, m + 1, lss);
            lss = strlen(s);
            if(lss != 0)
            {
                s[lss - 1] = '\n';
                s[lss] = '\0';
            }
            else
            {
                s[lss] = '\n';
                s[lss + 1] = '\0';
            }
            return 0;
        }
        if((str[i] == ' ') || (str[i] == '\t'))
        {
            if(fl == 0)
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                n = atoi(p);
                del_str(str, str, 0, i + 1);
                ls = strlen(str);
                ls = trim(str, ls);
                i = 0;
                fl = 1;
                free(p);
                continue;
            }
            else if(fl == 1)
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                m = atoi(p);
                str = del_str(str, str, 0, i + 1);
                ls = strlen(str);
                ls = trim(str, ls);
                free(p);
                if((strncmp(str, "\\n", 2) == 0) || (strncmp(str, "\'\\n\'", 4) == 0))
                {
                    fl = 2;
                    continue;
                }
                else if((strncmp(str, "\\0", 2) == 0) || (strncmp(str, "\'\\0\'", 4) == 0))
                {
                    fl = 3;
                    continue;
                }
                else if(strncmp(str, "\'\'", 2) == 0)
                {
                    return 0;
                    continue;
                }
                else if((strncmp(str, "\\t", 2) == 0) || (strncmp(str, "\'\\t\'", 4) == 0))
                {
                    c = '\t';
                    break;
                }
                else if((strncmp(str, "\\\\", 2) == 0) || (strncmp(str, "\'\\\\\'", 4) == 0))
                {
                    c = '\\';
                    break;
                }
                else if((strncmp(str, "\\\'", 2) == 0) || (strncmp(str, "\'\\\'\'", 4) == 0))
                {
                    c = '\'';
                    break;
                }
                else if(str[0] == '\'') 
                {
                    
                    if((str[2] == '\'') || (ls > 4 && (str[1] == '\\' && str[3] == '\'')))
                    {
                        for(j = findstr(str, "\'", 1); j < ls; j++)
                            if(str[j] == '#')
                            {
                                del_str(str, str, j, ls);
                                ls = strlen(str);
                                ls = trim(str, ls);
                                break;
                            }
                        if(ls > 5)
                        {
                            printf(ERR_COMM);
                            return 0;
                        }
                        if(str[1] == '\\' && str[3] == '\'') c = str[2];
                        else c = str[1];
                        break;
                    }
                    else
                    {
                        printf(ERR_COMM);
                        return 0;
                    }
                }
                else 
                {
                    for(j = 1; j < ls; j++)
                        if(str[j] == '#')
                        {
                            del_str(str, str, j, ls);
                            ls = strlen(str);
                            ls = trim(str, ls);
                            break;
                        }
                    if(ls > 2)
                    {
                        printf(ERR_COMM);
                        return 0;
                    }
                    c = str[0];
                    break;
                }
            }    
        }
        i++;
    }
        
    if (c == '\0')
    {
        printf(ERR_COMM);
        return 0;
    }
    if (n < 1) n = 1;
    if (n > lst->size) n = lst->size;
    s = get_el(lst, n - 1)->value;
    if(s == NULL) return 0;
    ls = strlen(s);
    m--;
    if(m < 0) m = 0;
    if(m > ls)
    {
        if(n != lst->size)
        {
            s[ls - 1] = c;
            s[ls] = '\n';
            s[ls + 1] = '\0';
        }
        else
        {
            s[ls] = c;
            s[ls + 1] = '\0';
        }
        return 0;
    }
    for(i = ls + 1; i > m; i--)
        s[i] = s[i - 1];
    s[m] = c;
    return 0;
}

char* replstr(char* dst, int num, const char* str, const char* orig, const char* rep)
{
    const char* ptr;
    size_t len1 = strlen(orig);
    size_t len2 = strlen(rep);
    char*  tmp  = dst;
    
    num -= 1;
    while((ptr = strstr(str, orig)) != NULL)
    {
        num -= (ptr - str) + len2;
        if(num < 1)
            break;
        strncpy(dst, str, (size_t)(ptr - str));
        dst += ptr - str;
        strncpy(dst, rep, len2);
        dst += len2;
        str  = ptr + len1;
    }

    for(; (*dst = *str) && (num > 0); --num)
    {
        ++dst;
        ++str;
    }
    return tmp;
}

int replbe(char *str, char* sws, char* shs)
{
    char *str2;
    int i = 0, j = 0;
    int ls = strlen(str), lshs = strlen(shs);
    str2 = malloc(ls + lshs);
    if(*sws == '^')
    {
        for (i = 0; i < lshs; i++)
            str2[i] = shs[i];
        j=lshs;
        for(i = 0; i < ls; i++)
        {
            str2[j] = str[i];
            j++;
        }
    }
    if(*sws == '$')
    {
        for(i = 0; i < ls; i++)
            str2[i] = str[i];
        j = ls;
        for(i = 0; i < lshs; i++)
        {
            str2[j] = shs[i];
            j++;
        }
    }
    str = strcpy(str, str2);
    free(str2);
  return 0;
}

int replace_substring(linf *lst, char *str)
{
    char *nn = NULL, *s2 = NULL/*, *ss = NULL*/;
    int i = 0, kk = 0, ffk = 0, fk = 0, j = 0;
    int n = 0, m = lst->size;
    int ls = strlen(str), num = 0;
    char *p = NULL;
    ls = trim(str, ls);
    if(ls < 2)
    {
        resub = 2;
        return 0;
    }
    if(resub == 0)
    {
        while(i <= ls - 1)
        {
            if((str[i] == ' ') || (str[i] == '\t'))
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                n = atoi(p);
                del_str(str, str, 0, i);
                ls = strlen(str);
                ls = trim(str, ls);
                free(p);
                if(str[0] == '"') break;
                i = 0;
                continue;
            }
            if(str[i] == '"')
            {
                p = malloc(ls - i + 1);
                del_str(p, str, i, ls);
                n = atoi(p);
                del_str(str, str, 0, i);
                ls = strlen(str);
                ls = trim(str, ls);
                free(p);
                break;
            }
            i++;
        }
        
        i = 0;
        if(ls < 2)
        {
            resub = 2;
            return 0;
        }
        switch(str[0])
        {
            case '"':
            {
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    printf(ERR_COMM);
                    return 0;
                }
                if(strncmp(str, "\"\"", 2) == 0)
                {
                    printf(ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                fk = findstr(str, "\"", 1);
                sw = malloc(ls - fk + 1);
                del_str(sw, str, fk, ls);
                del_str(str, str, 0, fk + 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if((kk < 2) || (str[0] != '"'))
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 2) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                break;
            }
            case '#':
            {
                resub = 2;
                return 0;
            }
            case '^':
            {
                sw = malloc(5);
                sw = "^";
                sw[1] = '\0';
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 2) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                break;
            }
            case '$':
            {
                sw = malloc(5);
                sw = "$";
                sw[1] = '\0';
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 2) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf(ERR_COMM);
                    return 0;
                }
                break;
            }
            default:
            {
                printf(ERR_COMM);
                return 0;
            }
        }
    }
    else if(resub == 1)
    {
        kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
        if(kk < 2)
        {
            printf("r=1,kk<2,%s",ERR_COMM);
            return 0;
        }
        if(strncmp(str, "\"\"", 2) == 0)
            ffk = 1;
        else
        {
            del_str(str, str, 0, 1);
            ls = strlen(str);
            while(1)
            {
                fk = findstr(str, "\"", i);
                if(fk >= 1 && str[fk - 1] == '\\')
                {
                    if((fk >= 0) && (str[fk - 2]) == '\\') break;
                    else i += fk - i + 1;
                }
                else break;
            }
            for(i = fk + 1; i < ls; i++)
                if(str[i] == '#')
                {
                    del_str(str, str, i, ls);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    break;
                }
            if(str[ls - 1] == '"')
            {
                sh = malloc(ls + 2);
                del_str(sh, str, fk, ls);
            }
            else
            {
                printf("r=1.sh.s=%s, %s", str,ERR_COMM);
                return 0;
            }
            resub = 0;
        }
    }
    else if(resub == 2)
    {
        switch(str[0])
        {
            case '"':
            {
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    printf("r=2.k<2, %s",ERR_COMM);
                    return 0;
                }
                if(strncmp(str, "\"\"", 2) == 0)
                {
                    printf("sw=\"\", %s",ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                fk = findstr(str, "\"", 1);
                sw = malloc(fk + 2);
                del_str(sw, str, fk, 1);
                del_str(str, str, 0, fk);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if((kk < 2) || (str[0] != '"'))
                {
                    free(sw);
                    az = 1;
                    printf("r=2.sw!=0,kk<2, %s",ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 0) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf("r=2.sh, s=%s, %s", str, ERR_COMM);
                    return 0;
                }
                break;
            }
            case '#':
            {
                resub = 2;
                return 0;
            }
             case '^':
            {
                sw = malloc(5);
                sw = "^";
                sw[1] = '\0';
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    free(sw);
                    printf("r=2.sw=^, kk<2,%s",ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 0) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf("r=2.sh. s=%s, %s", str, ERR_COMM);
                    return 0;
                }
                break;
            }
            case '$':
            {
                sw = malloc(5);
                sw = "$";
                sw[1] = '\0';
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if((ls < 2) || (str[0] == '#'))
                {
                    resub = 1;
                    return 0;
                }
                kk = fndsb(str, "\"") - fndsb(str, "\\\"") + fndsb(str, "\\\\\"");
                if(kk < 2)
                {
                    free(sw);
                    az = 1;
                    printf("r=2.sw=$, kk<2,%s",ERR_COMM);
                    return 0;
                }
                del_str(str, str, 0, 1);
                ls = strlen(str);
                ls = trim(str, ls);
                if(strncmp(str, "\"", 1) == 0)
                {
                    ffk = 1;
                    break;
                }
                while(1)
                {
                    fk = findstr(str, "\"", i);
                    if(fk >= 1 && str[fk - 1] == '\\')
                    {
                        if((fk >= 0) && (str[fk - 2]) == '\\') break;
                        else i += fk - i + 1;
                    }
                    else break;
                }
                for(i = fk + 1; i < ls; i++)
                    if(str[i] == '#')
                    {
                        del_str(str, str, i, ls);
                        ls = strlen(str);
                        ls = trim(str, ls);
                        break;
                    }
                if(str[ls - 1] == '"')
                {
                    sh = malloc(ls + 2);
                    del_str(sh, str, fk, ls);
                }
                else
                {
                    free(sw);
                    az = 1;
                    printf("r=2.sh. s=%s, %s", str, ERR_COMM);
                    return 0;
                }
                break;
            }
            default:
            {
                printf(ERR_COMM);
                return 0;
            }
        }
    }
    
    if(lst->size == 0)
    {
        printf("-The empty list-\n");
        return 0;
    }
    if(m <= 0) m = 1;
    if(m > lst->size)
        m = lst->size;
    n--;
    m--;
    if(n < 0) n = 0;
    if(n > lst->size - 1) n = lst->size - 1;
    if (n > m)
    {
        printf(ERR_COMM);
        return 0;
    }
    for(i = n; i <= m; i++)
    {
        char *s = NULL;
        int ii = 0, jj = 0;
        s = malloc( strlen(get_el(lst, i)->value) + 10);if (s == NULL) return 0;
        strcpy(s, get_el(lst, i)->value);if(strlen(s) >= 1 && s[strlen(s) -1] == '\n')  
            s[strlen(s) -1] = '\0';
        if(ffk == 0)
        {
            int ln = 0;
            if(fndsb(s, sw) == 0) {free(s); continue;}
            num = strlen(s) + fndsb(s, sw)*strlen(sh) + 50;
            if ((*sw != '^') && (*sw != '$'))
            {
                s2 = malloc(num);
                if(s2 == NULL) return 0;
                replstr(s2, num - 1, s, sw, sh);
                s2[num - 1] = '\0';
            }
            else replbe(s, sw, sh);
            nn = malloc(num+PIECE_SIZE);
            nn[0] = '\0';
            itoa(i + 1, nn);
            ln = strlen(nn);
            nn[ln] = ' ';
            nn[ln+1] = '\"';
            for(ii = ln+2, jj = 0; jj< strlen(s2); ii++)
            {
                nn[ii] = s2[jj];
                jj++;
            }
            nn[ii] = '\0';
            strcat(nn, "\"");
            insert_after(lst, nn);
            delete_el(lst, i);
            free(nn);
            free(s2);
        }
        else if(ffk == 1)
        {
            while(findstr(s, sw, j) != -1)
            {
                del_str(s, s, findstr(s, sw, j), findstr(s, sw, j) + strlen(sw));
                j += findstr(s, sw, j);
            }
            j = 0;
        }
        
        free(s);
    }
    free(sw);
    if(ffk == 0) free(sh);
    az = 1;
    return 0;
}

int delete_range(linf *lst, char *str) 
{
    int i = 0, fd = 0;  
    int n = 0, m = lst->size;
    int ls = strlen(str);
    char *p = NULL;
    ls = trim(str, ls);
    if(ls >= 2)
    {
        for(i = 0; i < ls; i++)
            if(str[i] == '#')
            {
                del_str(str, str, i, ls);
                ls = strlen(str);
                ls = trim(str, ls);
                fd = 1;
                break;
            }
        i = 0;
        if(ls >= 2)
            while(i <= ls - 1)
            {
                if((str[i] == ' ') || (str[i] == '\t'))
                {
                    p = malloc(ls - i + 1);
                    del_str(p, str, i, ls);
                    n = atoi(p);
                    del_str(str, str, 0, i + 1);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    free(p);
                    if((ls < 2)  && (fd == 0)) break;
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    m = atoi(str);
                    break;
                }
                if(str[i] == '\n')
                {
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    n = atoi(str);
                    break;
                }
                i++;
            }
    }
    
    if(lst->size == 0)
    {
        printf("-The empty list-\n");
        return 0;
    }
    if(m <= 0) m = 1;
    if(m > lst->size)
        m = lst->size;
    n--;
    m--;
    if (n < 0) n = 0;
    if (n > lst->size - 1) n = lst->size - 1;
    if (n > m)
    {
        printf(ERR_COMM);
        return 0;
    }
    for (i = n; i <= m; i++)
        delete_el(lst,i);  
    return 0;
}

int delete_braces(linf *lst, char *str) 
{
    int i = 0, fd = 0;
    char *s = NULL;
    int k1 = 0, k2 = 0, ks = 0, df = 0;
    int n = 0, m = lst->size;
    int ls = strlen(str), lss = 0;
    char *p = NULL;
    ls = trim(str, ls);
    if(ls >= 2)
    {
        for(i = 0; i < ls; i++)
            if(str[i] == '#')
            {
                del_str(str, str, i, ls);
                ls = strlen(str);
                ls = trim(str, ls);
                fd = 1;
                break;
            }
        i = 0;
        if(ls >= 2)
            while(i <= ls - 1)
            {
                if((str[i] == ' ') || (str[i] == '\t'))
                {
                    p = malloc(ls - i + 1);
                    del_str(p, str, i, ls);
                    n = atoi(p);
                    del_str(str, str, 0, i + 1);
                    ls = strlen(str);
                    ls = trim(str, ls);
                    free(p);
                    if((ls < 2)  && (fd == 0)) break;
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    m = atoi(str);
                    break;
                }
                if(str[i] == '\n')
                {
                    if(fd == 0) del_str(str, str, ls - 1, ls);
                    n = atoi(str);
                    break;
                }
                i++;
            }
    }
    
    if(lst->size == 0)
    {
        printf("-The empty list-\n");
        return 0;
    }
    if(m <= 0) m = 1;
    if(m > lst->size)
        m = lst->size;
    n--;
    m--;
    if(n < 0) n = 0;
    if(n > lst->size - 1) n = lst->size - 1;
    if (n > m)
    {
        printf(ERR_COMM);
        return 0;
    }
    i = n;
    while((i <= m) && (m >= 0))
    {
        s = get_el(lst, i)->value;
        lss = strlen(s);
        k1 = findstr(s, "{", 0);
        if(k1 == -1 && !ks)
        {
            i++;
            continue;
        }
        ks += fndsb(s, "{");
        k2 = findstr(s, "}", 0);
        if(k2 == -1 && ks > 0) 
        {
            if((k1 <= 0)) delete_el(lst, i);
            else 
            {
                del_str(s, s, k1, lss - 1);
                i++;
            }
            i--;
            m--;
            df = 1;
        }
        else if(k2 != -1 && ks > 0)
        {
            ks--;
            if(!ks)
            {
                if(lss < 3) delete_el(lst, i);
                else if(df == 1)del_str(s, s, 0, k2 + 1);
                else del_str(s, s, k1, k2 + 1);
                df = 0;
            }
            else
            {
                delete_el(lst, i);
                i--;
                m--;
            }
        }
        i++;
    }
    return 0;
}

int opnrdf(linf *lst, char *str)
{
    FILE* f;
    if(strlen(str) == 0)
    {
        printf(ERR_COMM);
        return 0;
    }
    f = fopen(str,"rt");
    if(f == NULL)
    {
        printf("-The program can not open the file: \"%s\"-\n", str);
        return OPEN_ERR;
    }
    lstdlt(lst);
    add_el(lst, f);
    fclose(f);
    return 0;
}

void add_l(linf *lst, FILE* f)
{
    int cnt = 0;
    List *el = lst->head;
    while (cnt < lst->size)
    {
        fprintf(f, "%s", el->value);
        el = el->next;
        cnt++;
    }
}

int wrt(linf *lst, char *str)
{
    FILE* f;
    if (strlen(str) == 0)
    {
        printf(ERR_COMM);
        return 0;
    }
    f = fopen(str, "w");
    if(f == NULL)
    {
        printf("-The program can not write to the file: \"%s\"-\n", str);
        return OPEN_ERR;
    }
    add_l(lst, f);
    fclose(f);
    return 0;
}

int get_comm(char *str)
{
    int c = 0;
    if(strncmp(str,"exit", strlen("exit"))==0) c=41;
    else if(strncmp(str,"help", strlen("help"))==0) c = 45;
    else if(strncmp(str,"read", strlen("read"))==0) c = 42;
    else if(strncmp(str,"open", strlen("open"))==0) c = 43;
    else if(strncmp(str,"write", strlen("write"))==0) c = 44;
    else if(strncmp(str,"print pages", strlen("print pages"))==0) c = 11;
    else if(strncmp(str,"print range", strlen("print range"))==0) c = 12;
    else if(strncmp(str,"insert after", strlen("insert after"))==0) c = 2;
    else if(strncmp(str,"edit string", strlen("edit string"))==0) c = 31;
    else if(strncmp(str,"insert symbol", strlen("insert symbol"))==0) c = 32;
    else if(strncmp(str,"replace substring", strlen("replace substring"))==0) c = 33;
    else if(strncmp(str,"delete range", strlen("delete range"))==0) c=34;
    else if(strncmp(str,"delete braces", strlen("delete braces"))==0) c=35;
    return c;   
}

void handler(int c)
{
    printf("\n--EDITOR is closed--\n");
    tcsetattr(0,TCSANOW,&old_attributes);
    free(nmf);
    lstdlt(lst);
    dltlst(&lst);
    if(help == 0)
    {
        lstdlt(lsth);
        dltlst(&lsth);
    }
    if(az == 0)
    {
        free(sw);
        free(sh);
    }
    free(strs);
    exit(0);
}

int main(int Arg, char** args)
{
    FILE* f;
    int c, ls = 0, nf = 0;
    char drive[BUF_SIZE];
    int size, len = 0;
    int add = 1, i = 0;
    tcgetattr(0,&old_attributes);
    memcpy(&new_attributes,&old_attributes,sizeof(struct termios));
    signal(SIGINT, handler);
    strs = malloc(BUF_SIZE);
    nmf = malloc(BUF_SIZE);
    lst = createlst();
    print_mode=NORMAL;
    if (Arg == 2)
    {
        f = fopen(args[1], "a+");
        if(f == NULL)
        {
            printf(ERR_OPEN_F);
            return OPEN_ERR;
        }
        add_el(lst, f);
        fclose(f);
    }
    else if (Arg > 1)
    {
        printf(ERR_ARG_COUNT);
        return ERR_QUANT_ARG;
    }
    printf("\n--EDITOR--\n\n");
    while (1)
    {
        printf("editor:\n");
        add = 1;
        len = 0;
        size = BUF_SIZE;
        if(fgets(drive, BUF_SIZE, stdin) == NULL)
            break;
        strs[0] = '\0';    
        for(len = 0; drive[len] != '\0'; len++);
        size -= len;
        do
        {
            add++;
            strs = realloc(strs, add*BUF_SIZE);
            if(strs == NULL)
                return ERR_ADDING_MEMORY;
            size += BUF_SIZE;
        } while(size <= 0);
        strs = strcpy(strs, drive);
        trim(strs, strlen(strs));
        if(strlen(strs) < 1 || strs[0] == '#' || strs[0] == '\n')
            continue;
        c = get_comm(strs);
        switch(c)
        {
            case 0:
            {
                printf(ERR_COMM);
                break;
            }
            case 11:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs); 
                        break;
                    }
                del_str(strs, strs, 0, strlen("print pages"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                if(ls >= 2)
                {
                    printf(ERR_COMM);
                    break;
                }
                if(lst->size == 0)
                {
                    printf("The file is NULL");
                    break;
                }
                printf("%c[2J",27);
                print_filecontent_lst_option(lst, 0, w.ws_row, 0, w.ws_col, 8);
                print_mode = PAGES;
                print_pages(lst);
                r = 1;
                tcsetattr(0,TCSANOW,&old_attributes);
                break;
            }
            case 12:
            {
                del_str(strs, strs, 0, strlen("print range"));
                print_range(lst, strs);
                break;
            }
            case 2:
            { 
                del_str(strs, strs, 0, strlen("insert after"));
                del_str(strs, strs, strlen(strs) - 1, strlen(strs));
                trim(strs, strlen(strs));
                insert_after(lst, strs);
                while(insaf == 1)
                {
                    add = 1;
                    len = 0;
                    size = BUF_SIZE;
                    fgets(drive, BUF_SIZE, stdin);
                    strs[0] = '\0';    
                    for(len = 0; drive[len] != '\0'; len++);
                    size -= len;
                    do
                    {
                        add++;
                        strs = realloc(strs, add*BUF_SIZE);
                        if(strs == NULL)
                            return ERR_ADDING_MEMORY;
                        size += BUF_SIZE;
                    } while(size <= 0);
                    strs = strcpy(strs, drive);
                    del_str(strs, strs, strlen(strs) - 1, strlen(strs));
                    insert_after(lst, strs);
                }
                nas = 0;
                break;
            }
            case 31:
            {  
                del_str(strs, strs, 0, strlen("edit strsing"));
                edit_string(lst, strs);
                break;
            }
            case 32:
            {
                del_str(strs, strs, 0, strlen("insert symbol"));
                insert_symbol(lst, strs);
                break;
            }
            case 33:
            {
                del_str(strs, strs, 0, strlen("replace substring"));
                del_str(strs, strs, strlen(strs) - 1, strlen(strs));
                trim(strs, strlen(strs));
                replace_substring(lst, strs);
                while(resub != 0)
                {
                    add = 1;
                    len = 0;
                    size = BUF_SIZE;
                    fgets(drive, BUF_SIZE, stdin);
                    strs[0] = '\0';    
                    for(len = 0; drive[len] != '\0'; len++);
                    size -= len;
                    do
                    {
                        add++;
                        strs = realloc(strs, add*BUF_SIZE);
                        if(strs == NULL)
                            return ERR_ADDING_MEMORY;
                        size += BUF_SIZE;
                    } while(size <= 0);
                    strs = strcpy(strs, drive);
                    del_str(strs, strs, strlen(strs) - 1, strlen(strs));
                    replace_substring(lst, strs);
                }
                break;
            }
            case 34:
            {
                del_str(strs, strs, 0, strlen("delete range"));
                delete_range(lst, strs);
                break;
            }
            case 35:
            {
                del_str(strs, strs, 0, strlen("delete braces"));
                delete_braces(lst, strs);
                break;
            }
            case 41:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs); 
                        break;
                    }
                del_str(strs, strs, 0, strlen("exit"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                if(ls >= 2)
                {
                    printf(ERR_COMM);
                    break;
                }
                printf("\n--EDITOR is closed--\n");
                free(nmf);
                lstdlt(lst);
                dltlst(&lst);
                free(strs);
                tcsetattr(0,TCSANOW,&old_attributes);
                return 0;
            }
            case 42:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs);
                        ls = trim(strs, ls);
                        break;
                    }
                del_str(strs, strs, 0, strlen("read"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                del_str(strs, strs, ls - 1, ls);
                trim(strs, ls - 1);
                opnrdf(lst, strs);
                print_range(lst, "");
                break;
            }
            case 43:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs);
                        ls = trim(strs, ls);
                        break;
                    }
                del_str(strs, strs, 0, strlen("open"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                if(ls == 1)
                {
                    printf(ERR_COMM);
                    break;
                }
                del_str(strs, strs, ls - 1, ls);
                ls = trim(strs, ls - 1);
                opnrdf(lst, strs);
                nmf = realloc(nmf, ls + 2);
                nmf[0] = '\0';
                nmf = strcpy(nmf, strs);
                nf = 1;
                print_range(lst, "");
                break;
            }
            case 44:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs);
                        ls = trim(strs, ls);
                        break;
                    }
                del_str(strs, strs, 0, strlen("write"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                if(ls > 1)
                {
                    del_str(strs, strs, ls - 1, ls);
                    ls = trim(strs, ls - 1);
                }
                if(ls == 1) 
                {
                    if(nf == 0) wrt(lst, args[1]);
                    else wrt(lst, nmf);
                }
                else wrt(lst, strs);
                break;
            }
            case 45:
            {
                ls = strlen(strs);
                for(i = 0; i < ls; i++)
                    if(strs[i] == '#')
                    {
                        del_str(strs, strs, i, ls);
                        ls = strlen(strs); 
                        break;
                    }
                del_str(strs, strs, 0, strlen("help"));
                ls = strlen(strs);
                ls = trim(strs, ls);
                if(ls >= 2)
                {
                    printf(ERR_COMM);
                    break;
                }
                lsth = createlst();
                help = 0;
                opnrdf(lsth, "help.txt");
                if(lsth->size == 0)
                {
                    lstdlt(lsth);
                    dltlst(&lsth);
                    printf("The file is NULL");
                    break;
                }
                printf("%c[2J",27);
                print_filecontent_lst_option(lsth, 0, w.ws_row, 0, w.ws_col, 8);
                print_mode = PAGES2;
                print_pages(lsth);
                r = 1;
                tcsetattr(0,TCSANOW,&old_attributes);
                lstdlt(lsth);
                dltlst(&lsth);
                help = 1;
                break;
            }
        }
        printf("\n");
    }
    free(nmf);
    lstdlt(lst);
    dltlst(&lst);
    tcsetattr(0,TCSANOW,&old_attributes);
    return 0;
}
