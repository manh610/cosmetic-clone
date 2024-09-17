package com.cosmetic.gg.dto.response.product;

import java.time.LocalDateTime;

import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ReviewResponse extends BaseModel{

	private String id;

	private String userId;

	private String productItemId;
	
	private String comment;
	
	private int star;
	
	private LocalDateTime createdAt;
	
	private String value;
}
