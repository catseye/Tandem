examplePrograms = [
    {
        "contents": "I\u00a0\u2192\u00a0cat\u00a0&\nQ\u00a0\u2192\u00a00\u00a0&\nO\u00a0\u2192\u00a0N\u00a0&\n(\n\u00a0\u00a0Q0\u00a0\u2192\u00a01\u00a0&\u00a0Ic\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a02\u00a0&\u00a0Ia\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a02\u00a0&\u00a0Io\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q2\u00a0\u2192\u00a03\u00a0&\u00a0It\u00a0\u2192\u00a0&\u00a0O\u2026\u00a0\u2192\u00a0Y\n)*\n",
        "filename": "fsa-accept.tandem"
    },
    {
        "contents": "I\u00a0\u2192\u00a0cab\u00a0&\nQ\u00a0\u2192\u00a00\u00a0&\nO\u00a0\u2192\u00a0N\u00a0&\n(\n\u00a0\u00a0Q0\u00a0\u2192\u00a01\u00a0&\u00a0Ic\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a02\u00a0&\u00a0Ia\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a02\u00a0&\u00a0Io\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q2\u00a0\u2192\u00a03\u00a0&\u00a0It\u00a0\u2192\u00a0&\u00a0O\u2026\u00a0\u2192\u00a0Y\n)*\n",
        "filename": "fsa-reject.tandem"
    },
    {
        "contents": "I \u2192 \"(()(()))\" &\nO\u00a0\u2192\u00a0N\u00a0&\nQ\u00a0\u2192\u00a00\u00a0&\nK\u00a0\u2192\u00a0\"$\"\u00a0&\n(\n\u00a0\u00a0Q0\u00a0\u2192\u00a01\u00a0&\u00a0I\"(\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\u2026\u00a0\u2192\u00a0\"$\"\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a01\u00a0&\u00a0I\"(\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\u2026\u00a0\u2192\u00a0X\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a01\u00a0&\u00a0I\")\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0KX\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a00\u00a0&\u00a0I\")\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\"$\"\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q0\u00a0\u2192\u00a02\u00a0&\u00a0I\u00a0\u2192\u00a0&\u00a0O\u2026\u00a0\u2192\u00a0Y\n)*\n",
        "filename": "pda-accept.tandem"
    },
    {
        "contents": "I \u2192 \"(()()()))\" &\nO\u00a0\u2192\u00a0N\u00a0&\nQ\u00a0\u2192\u00a00\u00a0&\nK\u00a0\u2192\u00a0\"$\"\u00a0&\n(\n\u00a0\u00a0Q0\u00a0\u2192\u00a01\u00a0&\u00a0I\"(\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\u2026\u00a0\u2192\u00a0\"$\"\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a01\u00a0&\u00a0I\"(\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\u2026\u00a0\u2192\u00a0X\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a01\u00a0&\u00a0I\")\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0KX\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q1\u00a0\u2192\u00a00\u00a0&\u00a0I\")\"\u2026\u00a0\u2192\u00a0\u2026\u00a0&\u00a0K\"$\"\u2026\u00a0\u2192\u00a0\u2026\u00a0|\n\u00a0\u00a0Q0\u00a0\u2192\u00a02\u00a0&\u00a0I\u00a0\u2192\u00a0&\u00a0O\u2026\u00a0\u2192\u00a0Y\n)*\n",
        "filename": "pda-reject.tandem"
    }
];