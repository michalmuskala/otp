#include <elf.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define ROUND_UP(v, a) (((v) + (a) - 1) & ~((size_t)(a) - 1))
#define PERROR(s) my_perror(__FILE__, __LINE__, s)

void
my_perror(const char *file, int line, const char *s)
{
	fprintf(stderr, "%s:%d %s: %s", file, line, s, strerror(errno));
}

/*
 * Helper function that opens and memory-maps the contents of a file.  Returns a
 * pointer to the mapping, the underlying file descriptor, and the file length.
 */
void *
load_file(const char *filename, int *fd, size_t *length)
{
	struct stat st;
	void *data;
	int flags;
	int prot;

	if ((*fd = open(filename, O_RDWR)) == -1) {
		PERROR("open");
		exit(EXIT_FAILURE);
	}
	if (fstat(*fd, &st) == -1) {
		PERROR("fstat");
		exit(EXIT_FAILURE);
	}
	*length = st.st_size;
	prot = PROT_READ | PROT_WRITE;
	flags = MAP_SHARED;
	if ((data = mmap(NULL, *length, prot, flags, *fd, 0)) == MAP_FAILED) {
		PERROR("mmap");
		exit(EXIT_FAILURE);
	}
	return data;
}

/*
 * A wrapper around the read(2) system call that retries for
 * interruptions.
 */
ssize_t
read_fully(int fd, char *buf, size_t nbytes)
{
	ssize_t n;

retry:
	if ((n = read(fd, buf, nbytes)) < 0) {
		if (errno == EINTR) {
			/* Interrupted before data was written */
			goto retry;
		}
		/* Another kind of error occurred */
		PERROR("write");
		exit(EXIT_FAILURE);
	}
	return n;
}


/*
 * A wrapper around the write(2) system call that retries for
 * interruptions and resumes partial writes.
 */
void
write_fully(int fd, char *buf, size_t count)
{
	ssize_t n;

retry:
	if ((n = write(fd, buf, count)) < 0) {
		if (errno == EINTR) {
			/* Interrupted before data was written */
			goto retry;
		} else {
			/* Another kind of error occurred */
			PERROR("write");
			exit(EXIT_FAILURE);
		}
	} else if (n < count) {
		/* Resume partial writes where they left off */
		count -= n;
		buf += n;
		goto retry;
	}
}

void
create_temporary_file(char *template)
{
	int fd;
	
	if ((fd = mkstemp(template)) == -1) {
		PERROR("mkstemp");
		exit(EXIT_FAILURE);
	}
	if (close(fd) == -1) {
		PERROR("close");
		exit(EXIT_FAILURE);
	}
}

void
copy_fd(int from, int to, off_t size)
{
	size_t buffer_size;
	struct stat st;
	void *buffer;
	ssize_t nbytes;

	/* pick a meaningful buffer size */
	buffer_size = 4096;
	if (fstat(from, &st) == -1) {
		PERROR("fstat");
		exit(EXIT_FAILURE);
	}
	if (st.st_blksize > buffer_size) {
		buffer_size = st.st_blksize;
	}
	if (fstat(to, &st) == -1) {
		PERROR("fstat");
		exit(EXIT_FAILURE);
	}
	if (st.st_blksize > buffer_size) {
		buffer_size = st.st_blksize;
	}

	/* allocate the copy buffer */
	if ((buffer = malloc(buffer_size)) == NULL) {
		PERROR("malloc");
		exit(EXIT_FAILURE);
	}

	/* do the copy */
	while (size > 0) {
		nbytes = read_fully(from, buffer, buffer_size);
		write_fully(to, buffer, nbytes);
		size -= nbytes;
	}

	/* deallocate the copy buffer */
	free(buffer);
}

void
copy_file(const char *from, const char *to)
{
	int fd[2];
	struct stat st;

	if ((fd[0] = open(from, O_RDONLY)) == -1) {
		PERROR("open");
		exit(EXIT_FAILURE);
	}
	if ((fd[1] = open(to, O_CREAT | O_TRUNC | O_WRONLY,  0777)) == -1) {
		PERROR("open");
		exit(EXIT_FAILURE);
	}
	if (fstat(fd[0], &st) == -1) {
		PERROR("fstat");
		exit(EXIT_FAILURE);
	}
	copy_fd(fd[0], fd[1], st.st_size);
	if (close(fd[0]) == -1) {
		PERROR("close");
		exit(EXIT_FAILURE);
	}
	if (close(fd[1]) == -1) {
		PERROR("close");
		exit(EXIT_FAILURE);
	}
}

void
rename_file(const char *src, const char *dst)
{
	if (rename(src, dst) == -1) {
		PERROR("unlink");
		exit(EXIT_FAILURE);
	}
}

/* Helper function that deletes a file mapping and closes its file descriptor */
void
unload_file(char *data, int fd, size_t length)
{
	if (munmap(data, length) == -1) {
		PERROR("munmap");
		exit(EXIT_FAILURE);
	}
	if (close(fd) == -1) {
		PERROR("close");
		exit(EXIT_FAILURE);
	}
}

/*
 * Compares the first four bytes of the object file to the ELF magic number.
 */
void
check_magic(void *object)
{
	const char magic[] = { 0x7F, 'E', 'L', 'F' };

	if (memcmp(object, magic, sizeof(magic)) == 0)
		return;

	fputs("Magic number mismatch\n", stderr);
	exit(EXIT_FAILURE);
}

/*
 * Returns the pointer to the ELF program header table
 */
Elf64_Phdr *
get_phdr(char *object, Elf64_Half *phnum)
{
	Elf64_Ehdr *ehdr;

	check_magic(object);
	ehdr = (Elf64_Ehdr *)object;
	*phnum = ehdr->e_phnum;
	return (Elf64_Phdr *)(object + ehdr->e_phoff);
}

/*
 * Rounds-up the file- and memory-size of the first and only loaded and
 * executable segment to a multiple of 2MiB.
 */
void
update_segments(char *data)
{
	Elf64_Phdr *phdr;
	Elf64_Half phnum;
	int i;
	int found_it;

	phdr = get_phdr(data, &phnum);
	for (i = 0, found_it = 0; i < phnum; i++, phdr++) {
		if (phdr->p_type == PT_LOAD && (phdr->p_flags & PF_X) != 0) {
			if (found_it) {
				fputs("Only one executable segment allowed\n",stderr);
				exit(EXIT_FAILURE);
			}
			phdr->p_filesz = ROUND_UP(phdr->p_filesz, 0x200000);
			phdr->p_memsz = ROUND_UP(phdr->p_filesz, 0x200000);
			found_it = 1;
		}
	}
}

/*
 * The main driver function for making a segment adjustment.
 */
void
adjust_copy(const char *ofilename, const char *nfilename)
{
	int fd;
	size_t length;
	char *object;

	copy_file(ofilename, nfilename);
	object = load_file(nfilename, &fd, &length);
	update_segments(object);
	unload_file(object, fd, length);
}

void
adjust_in_place(const char *filename)
{
	char template[] = "tmpXXXXXX";

	create_temporary_file(template);
	adjust_copy(filename, template);
	rename_file(template, filename);
}

void
usage(const char *argv0)
{
	fprintf(stderr, "%s: input-file [output-file]\n", argv0);
}

int
main(int argc, char **argv)
{
	switch (argc) {
	case 2:
		adjust_in_place(argv[1]);
		break;
	case 3:
		adjust_copy(argv[1], argv[2]);
		break;
	default:
		usage(argv[0]);
		return EXIT_FAILURE;
	}
	return 0;
}
