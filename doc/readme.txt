=Treebanking Overview=

[[pipeline.svg]]

This overview describes the process of transforming source (XML) files into treebanked files for the [[LDMT-MURI|link]] project. As the following image illustrates, this involves three high-level transformations and four high-level states.

We'll also provide information about the directory layout of the muri project, since the transformation scripts provided expect this structure:
    lang/                     # There is one tree for each language (currently kin=Kinyarwanda and mlg=Malagasy)
        orig/                 # The dir for source (XML) files
            collection1/      # There is one directory for each collection. Each directory contains all the documents in XML format.
            ...
            collectionN/
        tok/                  # The dir for tokenized files
            collections/      # Again, there is one dir for each collection.
            ...
        parsed/               # The dir for treebanked files
            src/              # This dir contains working files for treebanking
                collections/
                    document.lang/     # Each document is represented by a dir containing .tree files, with one tree per file
            collections/      # This contains the finished product of treebanking

=State 1: Source Files=
In this state, you create an XML file. We currently have not defined a schema for muri XML, but we do expect the XML to conform to the following structure:
    <file id="ID" languages="lang1,lang2,...">
        <data>
            <unit>
                <align>
                    <text langid="lang1">
                        <s>sentence</s>
                        ...
                    </text>
                    <text langid="lang2">...</text>
                    ...
                </align>
            <unit>
            ...
        </data>
    </file>
These files should be UTF-8 encoded, and they reside in the {lang}/orig/ directory. Note that each align block must contain exactly one text node for each of the languages specified in the file.languages attribute.

=Transition A: Tokenize=
This transformation turns a source file (name.xml) into a collection of token files (name.lang.tok). This transformation happens only rarely, when source files change. Also, it is fairly fast, so we don't tokenize single XML files. Instead, we tokenize all XML files for a language. Note that this operation is valid because there is no other way to create valid token files than through this transformation, and it is not permitted to manually edit token files.

Commands:
scalabha tokenize kin       # Tokenize all Kinyarwanda XML files
scalabha tokenize mlg       # Tokenize all Malagasy XML files
scalabha tokenize all       # Tokenize all XML files (just a shortcut for running both of the previous commands)

The output of this command is written to the {lang}/tok/ directory.

=State 2: Token Files=
During Transition A, each XML file is split into K token files, where K is the number of languages in the XML file (usually 2 or 3), named accordingly. The token files contain 1 line for each of the align blocks, and the sentences within the align blocks are delimited by <EOS> tags. These tags need to be removed before data releases, but they are neccessary for this process, so leave them for now.

As noted above, the only valid way to create or modify token files is first to create or modify the XML files and then run Transition A.This ensures that our token files always have the same set of known properties, which is important for machine translation.

=Transition B: TreeSeed=
This transformation turns a token file into a set of L tree files, where L is the number of lines in the token file. I.e., there is one tree per tree file. Note that the treeseed command checks the output files to make sure there are no midifications to them before overwriting, so if you have make changes but still want to create a new file, rename or delete the original before running treeseed.

Commands:
scalabha treeseed /path/to/filename.tok     # turn this token file into a set of tree files.

The output of this command is written to the {lang}/parsed/src/ directory.

=State 3: Treebank Files=
This state is where the brunt of the work occurs. The Treeseed transition creates dummy treebank files with the correct structure down to the sentence level, but you have to specify the sentence structure. All whitespace will be ignored, so you can format the file any way you like. Here is an example of a valid tree structure:
    (TOP
        (S
            (NP (N John) )
            (VP (V saw) 
                (NP (D the) (N saw) )
            )
            (.  .)
        )
        (S (NP (D The) (N saw) ) (VP (V sawed) (NP (D the) (N log) ) ) (. .) )
    )

Note that every tree's top level node is TOP, even if there is only one sentence. Also, note that tags must be ASCII characters, but the tokens can be UTF-8. Punctuation is treebanked by making it the last element in its syntactic structure (the sentence in this case) and giving it a tag ascii-equivalent to itself. In other words the tag for directional double quotes is the ascii double quote, etc.

=Transition C: TreeMerge=
This transition turns a collection of working tree files into a single output tree file. This means that whitespace in the input tree files will be collapsed, and each input tree will become a line in the output tree file. Note that input tree files are sorted alphabetically by name to determine the order of trees in the output file. Just as with Transition A, treemerge will overwrite the output files, so you should not make changes directly to the output tree files, only the ones in the src/ directory.

=State 4: Verification=
In this state, the process is complete, and all of the requisite files (XML, tok, and tree) exist. Here, you can use verification tools like tree-checker.pl to make sure that the output is well formed. You will also want to make sure of a few properties:
- For each tree file in parsed/*/*.tree, there is exactly one tok file in tok/*/*.tok. It is expected that there will be many token files that are not treebanked, but every treefile must correspond exactly to a token file.
- Likewise, each tree file must have exactly the same number of lines as its corresponding tok file. In fact, suppose that a source file orig/coll1/file_001.xml contains 2 languages, langA and langB. Then, the following 4 files must exist and must all have the same number of lines:
-- tok/coll1/file_001.langA.tok
-- tok/coll1/file_001.langB.tok
-- parsed/coll1/file_001.langA.tree
-- parsed/coll1/file_001.langB.tree

This is the state from which it is possible to do a data release.

=After: Data Release=
Before doing a data release, you'll want to re-run any validation tools you have at your disposal, as well as spot-checking the output.

Finally, as a cleanup, you should remove the <EOS> tags in the token files before packaging the data release. Just be sure to re-run the tokenizers (replaceing the <EOS> tags) before the next phase of treebanking begins. These tags are neccessary for the treeseed transition to create the correct sentence nodes, but they are not valid tokens, so they should not be present in released data.
