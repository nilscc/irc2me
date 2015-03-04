declare module "ByteBuffer" {

    function calculateVarint64(number) : number;
    function wrap(bytes : any) : Class;

    export class Class {

        offset : any;
        limit : any;

        constructor (totalLength : number);

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

}
