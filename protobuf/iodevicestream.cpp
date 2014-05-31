#include "iodevicestream.h"

/*
 * Input stream
 *
 */

IODeviceInputStream::IODeviceInputStream(QIODevice &dev)
    : dev(dev)
    , buffer(new char[MAX_BUFFER_LEN])
    , backed_up(0)
{
}

IODeviceInputStream::~IODeviceInputStream()
{
    delete buffer;
}

bool IODeviceInputStream::Next(const void **data, int *size)
{
    if (backed_up > 0)
    {
        // reuse old buffer data
        *data = buffer + backed_up;
        *size = backed_up;

        backed_up = 0;

        return true;
    }
    else
    {
        // read new data from the device
        int64_t len = dev.read(buffer, MAX_BUFFER_LEN);

        if (len > 0)
        {
            *size = len;
            *data = buffer;

            byte_count += len;

            return true;
        }
        else
            return false;
    }
}

void IODeviceInputStream::BackUp(int count)
{
    backed_up = count;
}

bool IODeviceInputStream::Skip(int count)
{
    if (backed_up > count)
    {
        // skip old data in buffer
        backed_up -= count;

        return true;
    }
    else
    {
        // skip "backed up" data
        count -= backed_up;
        backed_up = 0;

        bool ok = false;

        // read new data
        while (count > 0)
        {
            const void *dummy;
            int size;
            ok = Next(&dummy, &size);

            if (size > count)
            {
                BackUp(size - count);
            }
            else
                count -= size;
        }

        return ok;
    }
}

int64_t IODeviceInputStream::ByteCount()
{
    return byte_count;
}


/*
 * Output stream
 *
 */

IODeviceOutputStream::IODeviceOutputStream(QIODevice &dev)
    : dev(dev)
    , buffer(new char[MAX_BUFFER_LEN])
    , to_write(0)
{
}

IODeviceOutputStream::~IODeviceOutputStream()
{
    delete buffer;
}

bool IODeviceOutputStream::write()
{
    int total = 0;
    while (total < to_write)
    {
        int len = dev.write(buffer + total, to_write - total);

        // check for errors
        if (len < 0)
            return false;

        total += len;
    }

    byte_count += total;

    to_write = 0;
    return true;
}

bool IODeviceOutputStream::Next(void **data, int *size)
{
    // write current buffer to device
    if (!write())
        return false;

    *data = buffer;
    *size = MAX_BUFFER_LEN;
}

void IODeviceOutputStream::BackUp(int count)
{
    to_write -= count;
}

int64_t IODeviceOutputStream::ByteCount()
{
    return byte_count;
}



















