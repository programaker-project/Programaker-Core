function load_initial(workspace) {
    var initialXML = ('<xml xmlns="http://www.w3.org/1999/xhtml">' +
                        '<variables></variables>' +
                        '<block type="chat_whenreceivecommand" id="0(/MVL`$~*P5(O%5s8#3" x="153" y="94">' +
                          '<value name="VALUE">' +
                            '<shadow type="text" id=";:)sA9|K={h#x6#w:Gu:">' +
                              '<field name="TEXT">/start</field>' +
                            '</shadow>' +
                          '</value>' +
                          '<next>' +
                            '<block type="chat_say" id="O~sVPXF6IJ,jg0d@jV(E">' +
                              '<value name="VALUE">' +
                                '<shadow type="text" id="|F?og=a*Aw/ls|oG3joY">' +
                                  '<field name="TEXT">Hello!</field>' +
                                '</shadow>' +
                              '</value>' +
                              '<next>' +
                                '<block type="control_repeat" id="|AoIPI?l~ZV5sb=e*u!a">' +
                                  '<value name="TIMES">' +
                                    '<shadow type="math_whole_number" id="%0I=YYi0R40B1oa9T]]:">' +
                                      '<field name="NUM">3</field>' +
                                    '</shadow>' +
                                  '</value>' +
                                  '<statement name="SUBSTACK">' +
                                    '<block type="chat_say" id="uDyM42Cu2wj0cP6#%0p;">' +
                                      '<value name="VALUE">' +
                                        '<shadow type="text" id="H[;b%1{ck!.0]CsrTuNe">' +
                                          '<field name="TEXT">Are we ready?</field>' +
                                        '</shadow>' +
                                      '</value>' +
                                    '</block>' +
                                  '</statement>' +
                                  '<next>' +
                                    '<block type="chat_say" id="bZ-v]:Yp,3uR9b_H}O($">' +
                                      '<value name="VALUE">' +
                                        '<shadow type="text" id="OFxWMlEI_XtGS9cjmHX(">' +
                                          '<field name="TEXT">Bye!</field>' +
                                        '</shadow>' +
                                      '</value>' +
                                    '</block>' +
                                  '</next>' +
                                '</block>' +
                              '</next>' +
                            '</block>' +
                          '</next>' +
                        '</block>' +
                        '<block type="chat_whenreceivecommand" id="yDWo5LobTxX%!N~$GQ[*" x="420" y="183">' +
                          '<value name="VALUE">' +
                            '<shadow type="text" id="j2VB1F:+HvxR:!.9TcDV">' +
                              '<field name="TEXT">/hello</field>' +
                            '</shadow>' +
                          '</value>' +
                          '<next>' +
                            '<block type="chat_say" id="Ig!`m9btC)RVBh{4!P-X">' +
                              '<value name="VALUE">' +
                                '<shadow type="text" id="]`?bpfE,[~Cfs.qKA)Q/">' +
                                  '<field name="TEXT">Hi there</field>' +
                                '</shadow>' +
                              '</value>' +
                            '</block>' +
                          '</next>' +
                        '</block>' +
                      '</xml>');

    var xml = Blockly.Xml.textToDom(initialXML);
    Blockly.Xml.domToWorkspace(xml, workspace);
}
