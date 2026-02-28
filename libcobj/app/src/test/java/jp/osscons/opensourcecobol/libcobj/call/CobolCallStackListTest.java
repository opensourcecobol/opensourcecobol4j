/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.call;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class CobolCallStackListTest {

    @Test
    void testDefaultConstructor() {
        CobolCallStackList list = new CobolCallStackList();
        assertNull(list.getParent());
        assertNull(list.getChildren());
        assertNull(list.getSister());
        assertNull(list.getName());
    }

    @Test
    void testConstructorWithName() {
        CobolCallStackList list = new CobolCallStackList("TEST-PROGRAM");
        assertNull(list.getParent());
        assertNull(list.getChildren());
        assertNull(list.getSister());
        assertEquals("TEST-PROGRAM", list.getName());
    }

    @Test
    void testSetAndGetParent() {
        CobolCallStackList parent = new CobolCallStackList("PARENT");
        CobolCallStackList child = new CobolCallStackList("CHILD");

        child.setParent(parent);

        assertEquals(parent, child.getParent());
        assertEquals("PARENT", child.getParent().getName());
    }

    @Test
    void testSetAndGetChildren() {
        CobolCallStackList parent = new CobolCallStackList("PARENT");
        CobolCallStackList child = new CobolCallStackList("CHILD");

        parent.setChildren(child);

        assertEquals(child, parent.getChildren());
        assertEquals("CHILD", parent.getChildren().getName());
    }

    @Test
    void testSetAndGetSister() {
        CobolCallStackList first = new CobolCallStackList("FIRST");
        CobolCallStackList second = new CobolCallStackList("SECOND");

        first.setSister(second);

        assertEquals(second, first.getSister());
        assertEquals("SECOND", first.getSister().getName());
    }

    @Test
    void testHierarchicalStructure() {
        CobolCallStackList root = new CobolCallStackList("ROOT");
        CobolCallStackList child1 = new CobolCallStackList("CHILD1");
        CobolCallStackList child2 = new CobolCallStackList("CHILD2");
        CobolCallStackList grandChild = new CobolCallStackList("GRANDCHILD");

        root.setChildren(child1);
        child1.setParent(root);
        child1.setSister(child2);
        child2.setParent(root);
        child1.setChildren(grandChild);
        grandChild.setParent(child1);

        assertEquals("ROOT", root.getName());
        assertEquals("CHILD1", root.getChildren().getName());
        assertEquals("CHILD2", root.getChildren().getSister().getName());
        assertEquals("GRANDCHILD", root.getChildren().getChildren().getName());
        assertEquals(root, child1.getParent());
        assertEquals(root, child2.getParent());
        assertEquals(child1, grandChild.getParent());
    }

    @Test
    void testSetParentToNull() {
        CobolCallStackList parent = new CobolCallStackList("PARENT");
        CobolCallStackList child = new CobolCallStackList("CHILD");

        child.setParent(parent);
        assertEquals(parent, child.getParent());

        child.setParent(null);
        assertNull(child.getParent());
    }

    @Test
    void testSetChildrenToNull() {
        CobolCallStackList parent = new CobolCallStackList("PARENT");
        CobolCallStackList child = new CobolCallStackList("CHILD");

        parent.setChildren(child);
        assertEquals(child, parent.getChildren());

        parent.setChildren(null);
        assertNull(parent.getChildren());
    }

    @Test
    void testSetSisterToNull() {
        CobolCallStackList first = new CobolCallStackList("FIRST");
        CobolCallStackList second = new CobolCallStackList("SECOND");

        first.setSister(second);
        assertEquals(second, first.getSister());

        first.setSister(null);
        assertNull(first.getSister());
    }

    @Test
    void testEmptyName() {
        CobolCallStackList list = new CobolCallStackList("");
        assertEquals("", list.getName());
    }

    @Test
    void testNullName() {
        CobolCallStackList list = new CobolCallStackList(null);
        assertNull(list.getName());
    }

    @Test
    void testSisterChain() {
        CobolCallStackList first = new CobolCallStackList("FIRST");
        CobolCallStackList second = new CobolCallStackList("SECOND");
        CobolCallStackList third = new CobolCallStackList("THIRD");

        first.setSister(second);
        second.setSister(third);

        assertEquals("SECOND", first.getSister().getName());
        assertEquals("THIRD", first.getSister().getSister().getName());
        assertNull(third.getSister());
    }

    @Test
    void testComplexHierarchyWithMultipleSiblings() {
        CobolCallStackList root = new CobolCallStackList("ROOT");
        CobolCallStackList child1 = new CobolCallStackList("CHILD1");
        CobolCallStackList child2 = new CobolCallStackList("CHILD2");
        CobolCallStackList child3 = new CobolCallStackList("CHILD3");
        CobolCallStackList grandChild1 = new CobolCallStackList("GRANDCHILD1");
        CobolCallStackList grandChild2 = new CobolCallStackList("GRANDCHILD2");

        root.setChildren(child1);
        child1.setParent(root);
        child1.setSister(child2);
        child2.setParent(root);
        child2.setSister(child3);
        child3.setParent(root);

        child1.setChildren(grandChild1);
        grandChild1.setParent(child1);
        grandChild1.setSister(grandChild2);
        grandChild2.setParent(child1);

        assertEquals("CHILD1", root.getChildren().getName());
        assertEquals("CHILD2", root.getChildren().getSister().getName());
        assertEquals("CHILD3", root.getChildren().getSister().getSister().getName());
        assertEquals("GRANDCHILD1", child1.getChildren().getName());
        assertEquals("GRANDCHILD2", child1.getChildren().getSister().getName());
        assertEquals(root, child1.getParent());
        assertEquals(root, child2.getParent());
        assertEquals(root, child3.getParent());
        assertEquals(child1, grandChild1.getParent());
        assertEquals(child1, grandChild2.getParent());
    }

    @Test
    void testDeepNesting() {
        CobolCallStackList level1 = new CobolCallStackList("LEVEL1");
        CobolCallStackList level2 = new CobolCallStackList("LEVEL2");
        CobolCallStackList level3 = new CobolCallStackList("LEVEL3");
        CobolCallStackList level4 = new CobolCallStackList("LEVEL4");

        level1.setChildren(level2);
        level2.setParent(level1);
        level2.setChildren(level3);
        level3.setParent(level2);
        level3.setChildren(level4);
        level4.setParent(level3);

        assertEquals("LEVEL2", level1.getChildren().getName());
        assertEquals("LEVEL3", level1.getChildren().getChildren().getName());
        assertEquals("LEVEL4", level1.getChildren().getChildren().getChildren().getName());
        assertEquals(level3, level4.getParent());
        assertEquals(level2, level3.getParent());
        assertEquals(level1, level2.getParent());
    }
}
