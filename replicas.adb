            select
               accept Block_Generator (s : in Seconds_Count) do
                  declare
                     Created_Block : Block;
                  begin
                     Created_Block.Header        := Random_Block_Header;
                     Created_Block.Parent_Header := Chains.End_of_Longest_Blockchain.Header;
                     Created_Block.Height        := Chains.End_of_Longest_Blockchain.Height + 1;
                     Created_Block.Time_Stamp    := s;

                     Created_Block := Sign (Created_Block, Id);

                     -- Here the longest blockchain would be updated, and the block
                     -- would be added into the local Block set.
                     Chains.Blocks.Include (Created_Block.Header, Created_Block);
                     Chains.End_of_Longest_Blockchain := Created_Block;

                     -- After setting all features done, the block would be sent
                     -- to all replicas, or try to be sent.
                     for a in Authorities loop

                        -- To avoid deadlock, when sending out, replicas should
                        -- exclude itself. This idea is shared by Xinlu Dong.
                        if a /= Id then
                           select
                              delay Connection_Loss_Timeout;
                              Put_Line ("No sending connection from " & Authorities'Image (Id) & " to " & Authorities'Image (a));
                           then abort
                              Send (a, Created_Block);
                           end select;
                        end if;
                     end loop;
                  end;
               end Block_Generator;

            or
               accept Block_Query_Receiver (q : in Block_Query; r : out Blockchain) do
                  declare
                     Longest_Chain : Blockchain;
                  begin
                     Longest_Chain := Longest_Blockchain (Chains);

                     -- If one replica is querying from Natural'First to
                     -- Natural'Last, the longeset blockchain would be returned.
                     --
                     -- Or when a replica query only a specific range of chain,
                     -- as long as it does not beyond the whole range, the query
                     -- would be replied properly. Otherwise, it would return an
                     -- empty vector.
                     if q.Start_Height = Natural'First and then q.End_Height = Natural'Last
                     then
                        r := Longest_Chain;
                     elsif q.Start_Height <= Chains.End_of_Longest_Blockchain.Height
                       and then q.End_Height <= Chains.End_of_Longest_Blockchain.Height
                     then
                        for i in q.Start_Height .. q.End_Height loop
                           r.Append (Longest_Chain (i));
                        end loop;
                     else
                        r := Empty_Vector;
                     end if;
                  end;
               end Block_Query_Receiver;

            or
               accept Block_Receiver (b : in Block) do
                  declare
                     theChain  : Blockchain;
                  begin
                     -- Since including a block no matter it is good or bad is side-effect
                     -- free (the only thing matter here is about the longest chain),
                     -- we could always include one.
                     Chains.Blocks.Include (b.Header, b);

                     -- This part is inspired by Haowen Li, he shared his general idea
                     -- to me that Time could be employed to classify and force these
                     -- replica to behave properly.

                     -- Then I came up with my own idea, that is -
                     -- for odd second, replicas with odd Id could only query replicas with
                     -- even Id, and those whose Id is odd and larger than its; replicas with
                     -- even Id could only query those whose Id is even and larger than its.
                     --
                     -- Also, for even second, replicas with even Id could only query replicas
                     -- with odd Id, and those whose Id is even and less than it; replicas with
                     -- odd Id could only query those whose Id is odd and less than its.
                     for a in Authorities_First_Pos .. Authorities_Last_Pos loop
                        if (Integer (Current_Block_Time_Stamp) mod 2 = 1
                            and then
                              ((Authorities'Pos (Id) mod 2 = 1 and then a mod 2 = 0)
                               or else (Authorities'Pos (Id) mod 2 = 1 and then a mod 2 = 1 and then a > Authorities'Pos (Id))
                               or else (Authorities'Pos (Id) mod 2 = 0 and then a mod 2 = 0 and then a > Authorities'Pos (Id))))
                          or else
                            (Integer (Current_Block_Time_Stamp) mod 2 = 0
                             and then
                               ((Authorities'Pos (Id) mod 2 = 0 and then a mod 2 = 1)
                                or else (Authorities'Pos (Id) mod 2 = 0 and then a mod 2 = 1 and then a < Authorities'Pos (Id))
                                or else (Authorities'Pos (Id) mod 2 = 1 and then a mod 2 = 1 and then a < Authorities'Pos (Id))))

                        then
                           theChain := Empty_Vector;
                           select
                              delay Connection_Loss_Timeout;
                              Put_Line ("No query connection from " & Authorities'Image (Id) & " to " & Authorities'Image (Authorities'Val (a)));
                           then abort
                              Query (Authorities'Val (a), (Natural'First, Natural'Last), theChain);
                           end select;

                           -- If the result of querying is verified to be correct, the total chain would be
                           -- included in the local blocks.
                           if theChain /= Empty_Vector and then Validator (theChain) then
                              if theChain.Last_Element.Height >= Chains.End_of_Longest_Blockchain.Height then
                                 for i in theChain.First_Element.Height .. theChain.Last_Element.Height loop
                                    Chains.Blocks.Include (theChain (i).Header, theChain (i));
                                 end loop;
                                 Chains.End_of_Longest_Blockchain := theChain.Last_Element;
                              end if;
                           end if;
                        end if;
                     end loop;
                  end;

               end Block_Receiver;

            end select;
