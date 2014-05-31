#pragma once
#include "messagestream.h"

template <class Msg>
bool MessageStream::send(Msg msg, QString *errorMsg)
{
    std::string s;
    msg.SerializeToString(&s);

    // create new output stream in seperate block to force call on BackUp etc.
    {
        google::protobuf::io::CodedOutputStream out(ostream);
        out.WriteVarint64(s.length());
        out.WriteString(s);
    }

    return ostream->write(errorMsg);
}
