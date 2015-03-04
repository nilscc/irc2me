declare module "ByteBuffer" {

    class ByteBuffer {

        offset : any;
        limit : any;

        constructor (capacity? : number, littleEndian? : boolean, noAssert? : boolean);

        static calculateVarint64(number) : number;
        static wrap(bytes : any) : ByteBuffer;

        /*
         * Class members
         *
         */

        writeVarint64(varint64 : number) : void;
        readVarint64() : any;

        append(bytes : any, limit? : number) : void;
        clear() : void;
        reset() : void;
        mark() : void;

        remaining() : number;
        toArrayBuffer() : any;
    }

    export = ByteBuffer;
}
