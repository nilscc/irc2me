#pragma once

#include <google/protobuf/io/zero_copy_stream.h>
#include <QIODevice>

#define MAX_BUFFER_LEN 1024

class IODeviceInputStream : public google::protobuf::io::ZeroCopyInputStream
{
private:
    QIODevice &dev;
    char *buffer;

    int backed_up;
    int64_t byte_count;

public:
    IODeviceInputStream(QIODevice &dev);
    ~IODeviceInputStream();

    bool Next(const void **data, int *size);
    void BackUp(int count);
    bool Skip(int count);
    int64_t ByteCount();
};

class IODeviceOutputStream : public google::protobuf::io::ZeroCopyOutputStream
{
private:
    QIODevice &dev;
    char *buffer;

    int to_write;
    int64_t byte_count;

    bool write();

public:
    IODeviceOutputStream(QIODevice &dev);
    ~IODeviceOutputStream();

    bool Next(void **data, int *size);
    void BackUp(int count);
    int64_t ByteCount();
};
