package com.cosmetic.gg.model;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class BaseModel implements Serializable{

	private static final long serialVersionUID = -7018454715494160108L;
	
	private String id;
}
